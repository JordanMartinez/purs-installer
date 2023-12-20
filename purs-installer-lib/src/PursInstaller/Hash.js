import crypto from "node:crypto";

export const sha1HashImpl = (buf) => {
  const hashSum = crypto.createHash("sha1");
  hashSum.update(buf);
  return hashSum.digest("hex");
}
