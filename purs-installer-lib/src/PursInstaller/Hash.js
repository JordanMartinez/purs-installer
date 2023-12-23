import crypto from "node:crypto";

export const hashImpl = (algorithm, buf, digestEncoding) => {
  return crypto.createHash(algorithm).update(buf).digest(digestEncoding);
};
