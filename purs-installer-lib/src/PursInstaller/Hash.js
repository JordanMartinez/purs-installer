import crypto from "node:crypto";

export const hashImpl = (algorithm, buf) => {
  return crypto.createHash(algorithm).update(buf).digest("hex");
};
