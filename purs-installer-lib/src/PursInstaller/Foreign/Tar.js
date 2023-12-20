import { extract } from "tar";

export const extractPursToDirImpl = (file, cwd, pursFile, cb) => {
  extract({
    strip: 1,
    file,
    cwd,
    strict: true,
    filter: (path, _) => path.endsWith(pursFile),
  }, null, cb);
};

export const extractToDirImpl = (file, cwd, cb) => {
  extract({
    strip: 1,
    file,
    cwd,
    strict: true,
  }, null, cb);
}