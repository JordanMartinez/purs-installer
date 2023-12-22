import crypto from "node:crypto";

export const createHashImpl = (algorithm) => crypto.createHash(algorithm);
export const updateHashImpl = (buffer, hash) => hash.update(buffer);
export const digestHashImpl = (hash) => hash.digest();