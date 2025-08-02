
// ======= presets =======
const builtInPresets = {
  Dreamy: { slices: 12, warp: 60, feedback: 55 },
  Pulse: { slices: 6, warp: 15, feedback: 10 },
  Ethereal: { slices: 14, warp: 80, feedback: 70 },
  Retro: { slices: 8, warp: 35, feedback: 20 },
  Fluid: { slices: 10, warp: 45, feedback: 80 },
  Deep: { slices: 5, warp: 20, feedback: 90 },
  "Cosmic Bloom": { slices: 16, warp: 70, feedback: 90 },
  "Stellar Eye": { slices: 14, warp: 55, feedback: 85 }
};

function updateURL(params) {
  const u = new URL(window.location.href);
  Object.entries(params).forEach(([k,v]) => {
    if (v === null || v === undefined || v === "") u.searchParams.delete(k);
    else u.searchParams.set(k, v);
  });
  window.history.replaceState({}, "", u.toString());
}
function getURLParams() {
  const u = new URL(window.location.href);
  return {
    slices: parseInt(u.searchParams.get("slices")) || null,
    warp: parseFloat(u.searchParams.get("warp")) || null,
    feedback: parseFloat(u.searchParams.get("feedback")) || null,
    preset: u.searchParams.get("preset") || null,
  };
}

// ======= audio & analysis setup =======
const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
let sourceNode = null;
let analyser = null;
let dataArray = null;
let mag = null;
let prevSpectrum = null;

// reactive state
const onsetHistory = [];
let beatInterval = 0.5;
const beatIntervalEMA = 0.1;
let lastFullBeatTime = 0;
let beatIntensity = 0;
let centroid = 0;
let isPlaying = false;
let startTimestamp = 0;
let offsetWhenPaused = 0;
let bassPrev = null;
let hiPrev = null;
let bassEnv = 0, midEnv = 0, trebleEnv = 0;
const attack = 0.3;
const release = 0.08;

const beatDot = document.getElementById("beatDot");
const beatText = document.getElementById("beatText");
const timeLabel = document.getElementById("time");
const tempoLabel = document.getElementById("tempo");
const centroidLabel = document.getElementById("centroid");
const bassLabel = document.getElementById("bassVal");
const midLabel = document.getElementById("midVal");
const trebleLabel = document.getElementById("trebleVal");

// For recording
let mediaRecorder = null;
let recordedChunks = [];
let recordingStart = 0;

// recording destination
let recordDest = audioCtx.createMediaStreamDestination();

async function loadAudio(file) {
  const arrayBuf = await file.arrayBuffer();
  const audioBuf = await audioCtx.decodeAudioData(arrayBuf);
  if (sourceNode) sourceNode.disconnect();
  sourceNode = audioCtx.createBufferSource();
  sourceNode.buffer = audioBuf;
  analyser = audioCtx.createAnalyser();
  analyser.fftSize = 2048;
  const bufferLength = analyser.frequencyBinCount;
  dataArray = new Uint8Array(bufferLength);
  mag = new Float32Array(bufferLength);

  sourceNode.connect(analyser);
  analyser.connect(audioCtx.destination);
  if (recordDest) analyser.connect(recordDest);

  sourceNode.onended = () => { isPlaying = false; };
  startTimestamp = audioCtx.currentTime;
  offsetWhenPaused = 0;
  sourceNode.start(0);
  isPlaying = true;
  lastFullBeatTime = audioCtx.currentTime;
  prevSpectrum = null;
  onsetHistory.length = 0;
  beatInterval = 0.5;
  beatIntensity = 0;
  bassPrev = null;
  hiPrev = null;
  bassEnv = midEnv = trebleEnv = 0;
}

function getCurrentTime() {
  if (!isPlaying) return offsetWhenPaused;
  return audioCtx.currentTime - startTimestamp;
}

function envelope(prev, value) {
  if (value > prev) {
    return prev + (value - prev) * (1 - Math.exp(-1 * (1 / (attack * 60))));
  } else {
    return prev + (value - prev) * (1 - Math.exp(-1 * (1 / (release * 60))));
  }
}

function analyzeAudio() {
  if (!analyser) return;
  analyser.getByteFrequencyData(dataArray);
  for (let i = 0; i < dataArray.length; i++) mag[i] = dataArray[i] / 255;

  const now = getCurrentTime();

  // spectral centroid
  let num = 0, den = 0;
  for (let i = 0; i < mag.length; i++) {
    num += i * mag[i];
    den += mag[i] + 1e-9;
  }
  centroid = (num / den) / mag.length;
  centroidLabel.textContent = centroid.toFixed(3);

  // band energies
  const nyquist = audioCtx.sampleRate / 2;
  const binFreq = nyquist / mag.length;
  const bassLo = Math.floor(20 / binFreq);
  const bassHi = Math.ceil(250 / binFreq);
  const midLo = Math.floor(250 / binFreq);
  const midHi = Math.ceil(4000 / binFreq);
  const trebleLo = Math.floor(4000 / binFreq);

  let bE = 0, mE = 0, tE = 0;
  for (let i = bassLo; i < Math.min(bassHi, mag.length); i++) bE += mag[i] * mag[i];
  for (let i = midLo; i < Math.min(midHi, mag.length); i++) mE += mag[i] * mag[i];
  for (let i = trebleLo; i < mag.length; i++) tE += mag[i] * mag[i];
  bE = Math.sqrt(bE);
  mE = Math.sqrt(mE);
  tE = Math.sqrt(tE);

  bassEnv = envelope(bassEnv, bE);
  midEnv = envelope(midEnv, mE);
  trebleEnv = envelope(trebleEnv, tE);
  const norm = v => Math.tanh(v * 3.0);
  bassLabel.textContent = norm(bassEnv).toFixed(2);
  midLabel.textContent = norm(midEnv).toFixed(2);
  trebleLabel.textContent = norm(trebleEnv).toFixed(2);

  // full-spectrum flux for beat
  let fullFlux = 0;
  if (prevSpectrum) {
    for (let i = 0; i < mag.length; i++) {
      const diff = mag[i] - prevSpectrum[i];
      if (diff > 0) fullFlux += diff;
    }
  }
  prevSpectrum = mag.slice(0);
  onsetHistory.push({ time: now, flux: fullFlux });
  if (onsetHistory.length > 100) onsetHistory.shift();
  const fluxes = onsetHistory.map(o => o.flux);
  const meanFlux = fluxes.reduce((a, b) => a + b, 0) / fluxes.length;
  const variance = fluxes.reduce((a, b) => a + (b - meanFlux) ** 2, 0) / fluxes.length;
  const stdFlux = Math.sqrt(variance + 1e-9);
  const threshold = meanFlux + stdFlux * 1.1;

  if (fullFlux > threshold && now - lastFullBeatTime > 0.25) {
    if (lastFullBeatTime > 0) {
      const interval = now - lastFullBeatTime;
      beatInterval = beatInterval * (1 - beatIntervalEMA) + interval * beatIntervalEMA;
    }
    lastFullBeatTime = now;
    beatIntensity = Math.min(1, (fullFlux - meanFlux) / (stdFlux + 1e-9));
    showBeat();
  } else {
    beatIntensity *= 0.96;
  }

  // sub-band fluxes: bass and hi-hat
  let bassFlux = 0;
  if (bassPrev) {
    for (let i = bassLo; i < Math.min(bassHi, mag.length); i++) {
      const diff = mag[i] - bassPrev[i - bassLo];
      if (diff > 0) bassFlux += diff;
    }
  }
  bassPrev = mag.slice(bassLo, Math.min(bassHi, mag.length));

  let hiFlux = 0;
  const hiLo = Math.floor(4000 / binFreq);
  if (hiPrev) {
    for (let i = hiLo; i < mag.length; i++) {
      const diff = mag[i] - hiPrev[i - hiLo];
      if (diff > 0) hiFlux += diff;
    }
  }
  hiPrev = mag.slice(hiLo, mag.length);

  const bassPulse = Math.tanh(bassFlux * 5);
  const hiPulse = Math.tanh(hiFlux * 5);

  // overall energy (RMS)
  let energy = 0;
  for (let i = 0; i < mag.length; i++) energy += mag[i] * mag[i];
  energy = Math.sqrt(energy / mag.length);

  timeLabel.textContent = now.toFixed(2);

  window._reactiveData = {
    beatIntensity,
    beatPhase: beatInterval > 0 ? ((now - lastFullBeatTime) / beatInterval) % 1 : 0,
    bassPulse,
    hiPulse,
    energy,
    centroid,
    bassEnv: norm(bassEnv),
    midEnv: norm(midEnv),
    trebleEnv: norm(trebleEnv)
  };
}

function showBeat() {
  beatDot.style.background = "#fffa6a";
  beatText.textContent = "⭑";
  setTimeout(() => {
    beatDot.style.background = "#222";
    beatText.textContent = "";
  }, 120);
}

function togglePlayPause() {
  if (!sourceNode) return;
  if (isPlaying) {
    sourceNode.stop();
    offsetWhenPaused = audioCtx.currentTime - startTimestamp;
    isPlaying = false;
  } else {
    const buffer = sourceNode.buffer;
    sourceNode = audioCtx.createBufferSource();
    sourceNode.buffer = buffer;
    sourceNode.connect(analyser);
    analyser.connect(audioCtx.destination);
    if (recordDest) analyser.connect(recordDest);
    sourceNode.start(0, offsetWhenPaused);
    startTimestamp = audioCtx.currentTime - offsetWhenPaused;
    isPlaying = true;
  }
}

// ======= WebGL shader visualizer set-up =======
const canvas = document.getElementById("glcanvas");
const gl = canvas.getContext("webgl2");
if (!gl) alert("WebGL2 not supported");

function compileShader(src, type) {
  const s = gl.createShader(type);
  gl.shaderSource(s, src);
  gl.compileShader(s);
  if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) {
    console.error("Shader compile error:", gl.getShaderInfoLog(s));
    throw new Error("Shader compile failed");
  }
  return s;
}
function makeProgram(vs, fs) {
  const p = gl.createProgram();
  gl.attachShader(p, compileShader(vs, gl.VERTEX_SHADER));
  gl.attachShader(p, compileShader(fs, gl.FRAGMENT_SHADER));
  gl.linkProgram(p);
  if (!gl.getProgramParameter(p, gl.LINK_STATUS)) {
    console.error("Program link error:", gl.getProgramInfoLog(p));
    throw new Error("Link failed");
  }
  return p;
}

const quadVS = `#version 300 es
precision highp float;
in vec2 a_pos;
out vec2 v_uv;
void main(){
  v_uv = a_pos * 0.5 + 0.5;
  gl_Position = vec4(a_pos,0,1);
}`;

const quadFS = `#version 300 es
precision highp float;
out vec4 outColor;
in vec2 v_uv;
uniform float u_time;
uniform vec2 u_res;
uniform sampler2D u_prev;
uniform float u_beat;
uniform float u_beat_phase;
uniform float u_centroid;
uniform float u_slices;
uniform float u_warp;
uniform float u_feedback;
uniform float u_bass;
uniform float u_mid;
uniform float u_treble;
uniform float u_bass_pulse;
uniform float u_hi_pulse;
uniform float u_energy;

float hash(vec2 p) {
  return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123);
}

vec2 kaleido(vec2 p, float slices){
  float ang = atan(p.y,p.x);
  float rad = length(p);
  float sector = 2.0 * 3.1415926 / slices;
  ang = mod(ang + 3.1415926, sector);
  ang = abs(ang - sector * 0.5) - sector * 0.5;
  return vec2(cos(ang), sin(ang)) * rad;
}

vec2 domainWarp(vec2 p, float t){
  vec2 q = p + 0.3 * vec2(sin(p.y * 3.0 + t), cos(p.x * 3.0 - t));
  q += 0.1 * vec2(sin(5.0 * q.x + t * 1.3), cos(5.0 * q.y - t * 1.7));
  return mix(p, q, 0.6);
}

void main(){
  vec2 uv = v_uv * 2.0 - 1.0;
  float aspect = u_res.x / u_res.y;
  uv.x *= aspect;

  vec4 prev = texture(u_prev, v_uv);

  float beatGlow = smoothstep(0.8, 1.0, 1.0 - abs(u_beat_phase - 0.5) * 2.0);
  float dynamicWarp = 1.0 + beatGlow * 0.25;
  vec2 warped = domainWarp(uv * (1.0 + 0.3 * sin(u_time * 0.7) * dynamicWarp), u_time);
  vec2 k = kaleido(warped, u_slices);

  float ripple = sin(10.0 * length(k) - u_time * 2.5 + u_beat * 5.0);
  float rings = smoothstep(0.2, 0.0, abs(sin(5.0 * length(k) - u_time * 1.2)));
  float hueShift = fract(u_centroid + 0.2 * sin(u_time * 0.5 + u_beat * 2.0));

  vec3 base = vec3(
    0.5 + 0.5 * sin(u_time + k.x * 3.0 + k.y * 2.0),
    0.5 + 0.5 * sin(u_time * 1.3 + k.x * 2.5),
    0.5 + 0.5 * sin(u_time * 0.8 + k.y * 3.5)
  );
  base *= 0.5 + 0.5 * ripple * rings;

  float beatPulse = smoothstep(0.0, 1.0, u_beat) * 0.8;
  base += beatPulse * vec3(1.0, 0.9, 0.6);

  float bassSpike = pow(u_bass_pulse, 2.0) * exp(-length(uv) * 2.5);
  base += bassSpike * vec3(1.0, 0.6, 0.3);

  float shimmerSeed = fract(sin(dot(uv * 25.0, vec2(12.9898,78.233))) * 43758.5453 + u_time * 5.0);
  float shimmer = smoothstep(0.9, 1.0, shimmerSeed);
  base += shimmer * u_hi_pulse * 0.35;

  base *= mix(0.8, 1.3, u_energy);

  vec2 flow = normalize(vec2(
    sin(uv.y * 3.0 + u_time * 0.5 + u_mid * 5.0),
    cos(uv.x * 3.0 - u_time * 0.4 + u_treble * 5.0)
  ) * 0.8 + uv * 0.2);
  float particleSeed = hash(floor((uv + flow * 0.3) * 6.0 + u_time * 0.2));
  float particle = smoothstep(0.95, 1.0, fract(particleSeed + sin(u_time * 0.7) * 0.1));
  float particleMask = particle * u_bass * 1.2;
  vec3 flowColor = vec3(0.2, 0.6, 1.0) * particleMask;

  vec3 feedbackCol = prev.rgb * (u_feedback * 0.005);
  vec3 color = mix(base, feedbackCol + base * 0.3, 0.5);
  color += flowColor * 0.5;

  float r = length(uv);
  float centerMask = mix(0.2, 1.0, smoothstep(0.22, 0.25, r));
  color *= centerMask;
  float ring = exp(-pow((r - 0.4) * 8.0, 2.0));
  vec3 ringColor = vec3(0.7, 0.8, 1.0) * (0.8 + 0.2 * u_beat);
  color += ring * ringColor;

  float angle = hueShift * 6.28318;
  mat3 hueRot = mat3(
    0.299 + 0.701 * cos(angle) + 0.168 * sin(angle),
    0.587 - 0.587 * cos(angle) + 0.330 * sin(angle),
    0.114 - 0.114 * cos(angle) - 0.497 * sin(angle),
    0.299 - 0.299 * cos(angle) - 0.328 * sin(angle),
    0.587 + 0.413 * cos(angle) + 0.035 * sin(angle),
    0.114 - 0.114 * cos(angle) + 0.292 * sin(angle),
    0.299 - 0.3 * cos(angle) + 1.25 * sin(angle),
    0.587 - 0.588 * cos(angle) - 1.05 * sin(angle),
    0.114 + 0.886 * cos(angle) - 0.203 * sin(angle)
  );
  color = clamp(hueRot * color, 0.0, 1.0);

  float dist = length(uv);
  float vign = smoothstep(1.2, 0.7, dist);
  color *= vign;

  outColor = vec4(color,1.0);
}`;

const program = makeProgram(quadVS, quadFS);
gl.useProgram(program);

// quad setup
const quadBuf = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, quadBuf);
gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
  -1,-1, 1,-1, -1,1,
  -1,1, 1,-1, 1,1
]), gl.STATIC_DRAW);
const posLoc = gl.getAttribLocation(program, "a_pos");
gl.enableVertexAttribArray(posLoc);
gl.vertexAttribPointer(posLoc, 2, gl.FLOAT, false, 0, 0);

// uniform locations
const loc_time = gl.getUniformLocation(program, "u_time");
const loc_res = gl.getUniformLocation(program, "u_res");
const loc_prev = gl.getUniformLocation(program, "u_prev");
const loc_beat = gl.getUniformLocation(program, "u_beat");
const loc_beat_phase = gl.getUniformLocation(program, "u_beat_phase");
const loc_centroid = gl.getUniformLocation(program, "u_centroid");
const loc_slices = gl.getUniformLocation(program, "u_slices");
const loc_warp = gl.getUniformLocation(program, "u_warp");
const loc_feedback = gl.getUniformLocation(program, "u_feedback");
const loc_bass = gl.getUniformLocation(program, "u_bass");
const loc_mid = gl.getUniformLocation(program, "u_mid");
const loc_treble = gl.getUniformLocation(program, "u_treble");
const loc_bass_pulse = gl.getUniformLocation(program, "u_bass_pulse");
const loc_hi_pulse = gl.getUniformLocation(program, "u_hi_pulse");
const loc_energy = gl.getUniformLocation(program, "u_energy");

// ping-pong FBOs
function createFBO(size) {
  const tex = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, tex);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, size, size, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  const fbo = gl.createFramebuffer();
  gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);
  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, tex, 0);
  return { fbo, tex };
}

let ping = createFBO(1024);
let pong = createFBO(1024);

function resizeCanvas() {
  const dpr = window.devicePixelRatio || 1;
  const size = Math.min(window.innerWidth, window.innerHeight) * 0.9;
  canvas.width = canvas.height = Math.floor(size * dpr);
  canvas.style.width = canvas.style.height = Math.floor(size) + "px";
  gl.viewport(0,0,canvas.width, canvas.height);
}
window.addEventListener("resize", resizeCanvas);
resizeCanvas();

let startTime = performance.now();

function renderFrame() {
  requestAnimationFrame(renderFrame);
  const t = (performance.now() - startTime) * 0.001;
  analyzeAudio();

  const reactive = window._reactiveData || {};

  gl.bindFramebuffer(gl.FRAMEBUFFER, pong.fbo);
  gl.useProgram(program);
  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, ping.tex);
  gl.uniform1i(loc_prev, 0);
  gl.uniform1f(loc_time, t);
  gl.uniform2f(loc_res, canvas.width, canvas.height);
  gl.uniform1f(loc_beat, reactive.beatIntensity || 0);
  gl.uniform1f(loc_beat_phase, reactive.beatPhase || 0);
  gl.uniform1f(loc_centroid, reactive.centroid || 0);
  gl.uniform1f(loc_slices, parseFloat(document.getElementById("slices").value));
  gl.uniform1f(loc_warp, parseFloat(document.getElementById("warp").value));
  gl.uniform1f(loc_feedback, parseFloat(document.getElementById("feedback").value));
  gl.uniform1f(loc_bass, reactive.bassEnv ?? 0);
  gl.uniform1f(loc_mid, reactive.midEnv ?? 0);
  gl.uniform1f(loc_treble, reactive.trebleEnv ?? 0);
  gl.uniform1f(loc_bass_pulse, reactive.bassPulse || 0);
  gl.uniform1f(loc_hi_pulse, reactive.hiPulse || 0);
  gl.uniform1f(loc_energy, reactive.energy || 0);
  gl.drawArrays(gl.TRIANGLES, 0, 6);

  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  gl.activeTexture(gl.TEXTURE0);
  gl.bindTexture(gl.TEXTURE_2D, pong.tex);
  gl.uniform1i(loc_prev, 0);
  gl.uniform1f(loc_time, t + 10.0);
  gl.uniform1f(loc_beat, 0.0);
  gl.uniform1f(loc_beat_phase, reactive.beatPhase || 0);
  gl.uniform1f(loc_centroid, reactive.centroid || 0);
  gl.drawArrays(gl.TRIANGLES, 0, 6);

  [ping, pong] = [pong, ping];
  timeLabel.textContent = getCurrentTime().toFixed(2);
}
requestAnimationFrame(renderFrame);

// ======= UI wiring =======
const slicesEl = document.getElementById("slices");
const warpEl = document.getElementById("warp");
const feedbackEl = document.getElementById("feedback");
const slicesVal = document.getElementById("slicesVal");
const warpVal = document.getElementById("warpVal");
const feedbackVal = document.getElementById("feedbackVal");
const presetSelect = document.getElementById("presetSelect");
const applyPresetBtn = document.getElementById("applyPreset");
const saveCustomBtn = document.getElementById("saveCustom");
const customNameInput = document.getElementById("customName");
const customListDiv = document.getElementById("customList");
const copyLinkBtn = document.getElementById("copyLink");
const linkStatus = document.getElementById("linkStatus");
const playPauseBtn = document.getElementById("playPause");
const recordBtn = document.getElementById("recordBtn");
const recStatus = document.getElementById("recStatus");
const recDot = document.getElementById("recDot");

function reflectControlValues() {
  slicesVal.textContent = slicesEl.value;
  warpVal.textContent = warpEl.value;
  feedbackVal.textContent = feedbackEl.value;
}
slicesEl.addEventListener("input", () => { reflectControlValues(); updateURL({slices: slicesEl.value, preset: ""}); });
warpEl.addEventListener("input", () => { reflectControlValues(); updateURL({warp: warpEl.value, preset: ""}); });
feedbackEl.addEventListener("input", () => { reflectControlValues(); updateURL({feedback: feedbackEl.value, preset: ""}); });

function populateBuiltins() {
  for (const name in builtInPresets) {
    const o = document.createElement("option");
    o.value = name;
    o.textContent = name;
    presetSelect.appendChild(o);
  }
}
populateBuiltins();

function applyPreset(name, obj) {
  slicesEl.value = obj.slices;
  warpEl.value = obj.warp;
  feedbackEl.value = obj.feedback;
  reflectControlValues();
  presetSelect.value = name;
  updateURL({ slices: obj.slices, warp: obj.warp, feedback: obj.feedback, preset: name });
}

applyPresetBtn.addEventListener("click", () => {
  const name = presetSelect.value;
  if (name && builtInPresets[name]) applyPreset(name, builtInPresets[name]);
});

function loadCustomPresets() {
  const stored = JSON.parse(localStorage.getItem("kaleido_presets") || "{}");
  customListDiv.innerHTML = "";
  for (const key in stored) {
    const wrapper = document.createElement("div");
    wrapper.className = "preset-row";
    const pill = document.createElement("div");
    pill.className = "pill";
    pill.textContent = key;
    const loadBtn = document.createElement("button");
    loadBtn.textContent = "Load";
    loadBtn.style.fontSize="0.6rem";
    loadBtn.addEventListener("click", () => applyPreset(key, stored[key]));
    const delBtn = document.createElement("button");
    delBtn.textContent="×";
    delBtn.style.fontSize="0.6rem";
    delBtn.addEventListener("click", () => {
      delete stored[key];
      localStorage.setItem("kaleido_presets", JSON.stringify(stored));
      loadCustomPresets();
    });
    wrapper.append(pill, loadBtn, delBtn);
    customListDiv.appendChild(wrapper);
  }
}
loadCustomPresets();
saveCustomBtn.addEventListener("click", () => {
  const name = customNameInput.value.trim();
  if (!name) return;
  const stored = JSON.parse(localStorage.getItem("kaleido_presets") || "{}");
  stored[name] = {
    slices: parseInt(slicesEl.value),
    warp: parseFloat(warpEl.value),
    feedback: parseFloat(feedbackEl.value)
  };
  localStorage.setItem("kaleido_presets", JSON.stringify(stored));
  loadCustomPresets();
  customNameInput.value="";
});

copyLinkBtn.addEventListener("click", () => {
  const u = new URL(window.location.href);
  u.searchParams.set("slices", slicesEl.value);
  u.searchParams.set("warp", warpEl.value);
  u.searchParams.set("feedback", feedbackEl.value);
  if (presetSelect.value) u.searchParams.set("preset", presetSelect.value);
  else u.searchParams.delete("preset");
  navigator.clipboard.writeText(u.toString()).then(() => {
    linkStatus.textContent = "Copied!";
    setTimeout(()=> linkStatus.textContent="Ready", 1200);
  });
  window.history.replaceState({}, "", u.toString());
});

function applyURLParams() {
  const params = getURLParams();
  if (params.preset && builtInPresets[params.preset]) {
    applyPreset(params.preset, builtInPresets[params.preset]);
  }
  if (params.slices !== null) slicesEl.value = params.slices;
  if (params.warp !== null) warpEl.value = params.warp;
  if (params.feedback !== null) feedbackEl.value = params.feedback;
  reflectControlValues();
}
applyURLParams();

const audioFileInput = document.getElementById("audioFile");
document.getElementById("loadBtn").addEventListener("click", () => audioFileInput.click());
audioFileInput.addEventListener("change", async e => {
  const f = e.target.files[0];
  if (!f) return;
  if (audioCtx.state === "suspended") await audioCtx.resume();
  await loadAudio(f);
});
window.addEventListener("dragover", e => { e.preventDefault(); });
window.addEventListener("drop", async e => {
  e.preventDefault();
  if (e.dataTransfer.files.length) {
    const f = e.dataTransfer.files[0];
    if (f.type.startsWith("audio/")) {
      if (audioCtx.state === "suspended") await audioCtx.resume();
      await loadAudio(f);
    }
  }
});
playPauseBtn.addEventListener("click", () => {
  if (audioCtx.state === "suspended") audioCtx.resume();
  togglePlayPause();
});

// recording helpers
function makeCombinedStream() {
  const canvasStream = canvas.captureStream(60);
  const audioStream = recordDest.stream;
  return new MediaStream([
    ...canvasStream.getVideoTracks(),
    ...audioStream.getAudioTracks()
  ]);
}

function startRecording() {
  if (mediaRecorder && mediaRecorder.state === "recording") return;
  const combined = makeCombinedStream();
  recordedChunks = [];
  try {
    mediaRecorder = new MediaRecorder(combined, { mimeType: "video/webm;codecs=vp9,opus" });
  } catch (e) {
    mediaRecorder = new MediaRecorder(combined);
  }
  mediaRecorder.ondataavailable = e => {
    if (e.data && e.data.size > 0) recordedChunks.push(e.data);
  };
  mediaRecorder.onstart = () => {
    recordingStart = performance.now();
    recStatus.textContent = "Recording...";
    recDot.style.visibility = "visible";
    recordBtn.textContent = "Stop";
    recordBtn.classList.add("rec-active");
    if (analyser) analyser.connect(recordDest);
  };
  mediaRecorder.onstop = () => {
    recDot.style.visibility = "hidden";
    recordBtn.textContent = "Record";
    recordBtn.classList.remove("rec-active");
    recStatus.textContent = "Processing...";
    const blob = new Blob(recordedChunks, { type: "video/webm" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    const ts = new Date().toISOString().replace(/[:.]/g, "-");
    a.download = `kaleido_capture_${ts}.webm`;
    a.href = url;
    a.textContent = "Download video";
    a.style.display = "inline-block";
    a.style.marginLeft = "8px";
    recStatus.textContent = "Done.";
    if (!document.getElementById("downloadLink")) {
      a.id = "downloadLink";
      recStatus.parentNode.appendChild(a);
    } else {
      const prev = document.getElementById("downloadLink");
      prev.replaceWith(a);
      a.id = "downloadLink";
    }
  };
  mediaRecorder.start(100);
}

function stopRecording() {
  if (mediaRecorder && mediaRecorder.state === "recording") mediaRecorder.stop();
}
recordBtn.addEventListener("click", () => {
  if (mediaRecorder && mediaRecorder.state === "recording") stopRecording();
  else {
    if (audioCtx.state === "suspended") audioCtx.resume();
    startRecording();
  }
});

reflectControlValues();
