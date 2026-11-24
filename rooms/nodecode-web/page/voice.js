// Voice input, the composer's microphone: the browser records with its own
// MediaRecorder, and the recording goes to /api/transcribe, where the engine a
// channel's voice message goes through reads it (the channel kit's
// transcription section). The words land in the box at the caret, to be read
// over before they are sent: speaking never sends anything to the session.

// What the recording is made as, the first the browser can make that the
// gateway sniffs (webm and ogg hold Opus; Safari makes only mp4). "" leaves
// the browser its own default; null is a browser that cannot record.
const TYPES = ["audio/webm;codecs=opus", "audio/ogg;codecs=opus", "audio/webm", "audio/mp4"];

export function recordingType() {
  if (typeof MediaRecorder === "undefined" || !navigator.mediaDevices?.getUserMedia) return null;
  return TYPES.find((type) => MediaRecorder.isTypeSupported(type)) ?? "";
}

// Opens the microphone and starts recording: resolves once it records, to
// { seconds(), stop() -> Promise<Blob>, cancel() }. ON-LEVEL hears the voice's
// loudness, 0 to 1, each frame while it records. Rejects as getUserMedia does:
// NotAllowedError for a microphone the person or the system refused,
// NotFoundError for none.
export async function startRecording({ onLevel } = {}) {
  const stream = await navigator.mediaDevices.getUserMedia({ audio: { echoCancellation: true, noiseSuppression: true } });
  const type = recordingType();
  let recorder;
  try {
    recorder = new MediaRecorder(stream, type ? { mimeType: type } : undefined);
  } catch (error) {
    for (const track of stream.getTracks()) track.stop();
    throw error;
  }
  const chunks = [];
  recorder.addEventListener("dataavailable", (event) => { if (event.data.size) chunks.push(event.data); });
  let context = null;
  let frame = 0;
  if (onLevel) {
    try {
      context = new AudioContext();
      const analyser = context.createAnalyser();
      analyser.fftSize = 512;
      context.createMediaStreamSource(stream).connect(analyser);
      const samples = new Uint8Array(analyser.fftSize);
      const tick = () => {
        analyser.getByteTimeDomainData(samples);
        let peak = 0;
        for (const sample of samples) peak = Math.max(peak, Math.abs(sample - 128));
        onLevel(peak / 128);
        frame = requestAnimationFrame(tick);
      };
      tick();
    } catch {
      context = null;
    }
  }
  // The microphone is let go the moment the recording ends: the system's
  // in-use light goes out with it.
  const release = () => {
    cancelAnimationFrame(frame);
    context?.close().catch(() => {});
    for (const track of stream.getTracks()) track.stop();
  };
  const ended = new Promise((resolve) => recorder.addEventListener("stop", () => {
    release();
    resolve(new Blob(chunks, { type: recorder.mimeType || type || "audio/webm" }));
  }, { once: true }));
  const started = performance.now();
  recorder.start(250);
  return {
    seconds: () => (performance.now() - started) / 1000,
    stop: () => {
      if (recorder.state !== "inactive") recorder.stop();
      return ended;
    },
    cancel: () => {
      chunks.length = 0;
      if (recorder.state !== "inactive") recorder.stop();
    },
  };
}
