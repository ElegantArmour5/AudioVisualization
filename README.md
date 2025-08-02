# Shader Kaleidoscope Web App (Separated)

### Structure
- `index.html` — main entry, references external CSS and JS.
- `style.css` — all styling.
- `main.js` — logic: audio loading/analysis, WebGL shader visualization, presets, recording/export, UI wiring.

### Usage
1. Open `index.html` in a modern browser (Chrome/Edge/Firefox).
2. Load an MP3/WAV by clicking or dragging.
3. Adjust presets/parameters, play audio, record WebM.

### Notes
- The “Cosmic Bloom” and “Stellar Eye” presets are included.
- Shareable URL encodes visual parameters (audio file not embedded).
- Export to MP4 with ffmpeg if desired:
  ```
  ffmpeg -i kaleido_capture_<timestamp>.webm -c:v libx264 -crf 18 -c:a aac output.mp4
  ```
