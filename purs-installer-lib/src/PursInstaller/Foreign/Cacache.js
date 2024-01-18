import cacache from "cacache";

export const getImpl = (cachePath, key) => cacache.get(cachePath, key);
export const putStreamImpl = (cachePath, key, options) => cacache.put.stream(cachePath, key, options);
export const infoImpl = (cachePath, key) => cacache.get.info(cachePath, key);
export const rmEntryImpl = (cachePath, key) => cacache.rm.entry(cachePath, key);
export const verifyImpl = (cachePath) => cacache.verify(cachePath);
export const lsImpl = (cachePath) => cacache.ls(cachePath);