const undefinedImpl = undefined
export { undefinedImpl as undefined }

export const pseudoMap = f => x => (x === undefined) ? undefined : f(x)