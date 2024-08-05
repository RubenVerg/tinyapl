export const encode = (audioData, float32 = true, symmetric = false) => {
    const createWriter = (view) => {
        let pos = 0;
        return {
            int16(value) { view.setInt16(pos, value, true); pos += 2; },
            uint16(value) { view.setUint16(pos, value, true); pos += 2; },
            uint32(value) { view.setUint32(pos, value, true); pos += 4; },
            string(value) {
                for (const ch of value)
                    view.setUint8(pos++, ch.charCodeAt(0));
            },
            pcm8(value) {
                value = Math.max(-1, Math.min(value, 1));
                value = (value * 0.5 + 0.5) * 255;
                value = Math.round(value) | 0;
                view.setUint8(pos++, value);
            },
            pcm8s(value) {
                value = Math.round(value * 128) + 128;
                value = Math.max(0, Math.min(value, 255));
                view.setUint8(pos++, value);
            },
            pcm16(value) {
                value = Math.max(-1, Math.min(value, 1));
                value = value < 0 ? value * 32768 : value * 32767;
                value = Math.round(value) | 0;
                view.setInt16(pos, value, true);
                pos += 2;
            },
            pcm16s(value) {
                value = Math.round(value * 32768);
                value = Math.max(-32768, Math.min(value, 32767));
                view.setInt16(pos, value, true);
                pos += 2;
            },
            pcm24(value) {
                value = Math.max(-1, Math.min(value, 1));
                value = value < 0 ? 0x1000000 + value * 8388608 : value * 8388607;
                value = Math.round(value) | 0;
                const x0 = (value >> 0) & 0xff;
                const x1 = (value >> 8) & 0xff;
                const x2 = (value >> 16) & 0xff;
                view.setUint8(pos++, x0);
                view.setUint8(pos++, x1);
                view.setUint8(pos++, x2);
            },
            pcm24s(value) {
                value = Math.round(value * 8388608);
                value = Math.max(-8388608, Math.min(value, 8388607));
                const x0 = (value >> 0) & 0xff;
                const x1 = (value >> 8) & 0xff;
                const x2 = (value >> 16) & 0xff;
                view.setUint8(pos++, x0);
                view.setUint8(pos++, x1);
                view.setUint8(pos++, x2);
            },
            pcm32(value) {
                value = Math.max(-1, Math.min(value));
                value = value < 0 ? value * 2147483648 : value * 2147483647;
                value = Math.round(value) | 0;
                view.setInt32(pos, value, true);
                pos += 4;
            },
            pcm32s(value) {
                value = Math.round(value * 2147483648);
                value = Math.max(-2147483648, Math.min(value, +2147483647));
                view.setInt32(pos, value, true);
                pos += 4;
            },
            pcm32f(value) {
                view.setFloat32(pos, value, true);
                pos += 4;
            },
        };
    };
    const channelLength = audioData.channelData[0].length;
    const channels = audioData.channelData.length;
    const bitDepth = float32 ? 32 : 16;
    const bytes = bitDepth >> 3;
    const length = channelLength * channels * bytes;
    const view = new DataView(new Uint8Array(44 + length).buffer);
    const writer = createWriter(view);
    writer.string('RIFF');
    writer.uint32(view.buffer.byteLength - 8);
    writer.string('WAVE');
    writer.string('fmt ');
    writer.uint32(16);
    writer.uint16(float32 ? 3 : 1);
    writer.uint16(channels);
    writer.uint32(audioData.sampleRate);
    writer.uint32(audioData.sampleRate * channels * bytes);
    writer.uint16(channels * bytes);
    writer.uint16(bitDepth);
    const encoderOption = float32 ? 'f' : symmetric ? 's' : '';
    const methodName = 'pcm' + bitDepth + encoderOption;
    if (!(methodName in writer))
        throw TypeError(`Unsupported bit depth ${bitDepth}`);
    const write = writer[methodName].bind(writer);
    writer.string('data');
    writer.uint32(length);
    for (let i = 0; i < channelLength; i++)
        for (let ch = 0; ch < channels; ch++)
            write(audioData.channelData[ch][i]);
    return view.buffer;
};
