import type { FromElm } from './elm-ts-interop'

export function assert(expr: boolean, message: string): asserts expr is true {
  if (!expr) {
    throw new Error(message)
  }
}

export function assertIsArray(
  unknown: unknown,
  msg: string,
): asserts unknown is Array<unknown> {
  assert(Array.isArray(unknown), msg)
}

export function assertIsString(
  unknown: unknown,
  msg: string,
): asserts unknown is string {
  assert(typeof unknown === 'string', msg)
}

export function assertIsNumber(
  unknown: unknown,
  msg: string,
): asserts unknown is number {
  assert(typeof unknown === 'number', msg)
}

export function assertIsStringArray(
  unknown: unknown,
  msg: string,
): asserts unknown is Array<string> {
  assertIsArray(unknown, msg)

  unknown.forEach((entry) => {
    assertIsString(entry, msg)
  })
}

export const isObject = (unknown: unknown): unknown is object =>
  unknown !== null && typeof unknown === 'object'

export const hasKey = <K extends string>(
  unknown: unknown,
  key: K,
): unknown is Record<K, unknown> => isObject(unknown) && key in unknown

export const hasKeys = <K extends string>(
  unknown: unknown,
  ...keys: Array<K>
): unknown is Record<K, unknown> =>
  keys.every((key): boolean => hasKey(unknown, key))

export const assertHasKey: <K extends string>(
  unknown: unknown,
  key: K,
  msg: string,
) => asserts unknown is Record<K, unknown> = (unknown, key, msg) =>
  assert(hasKey(unknown, key), msg)

export const isUint8Array = (unknown: unknown): unknown is Uint8Array =>
  unknown instanceof Uint8Array

export const assertIsUint8Array: (
  unknown: unknown,
  msg: string,
) => asserts unknown is Uint8Array = (
  unknown: unknown,
  msg: string,
): asserts unknown is Uint8Array => assert(isUint8Array(unknown), msg)

export type UnionToData<Union, Tag> = Union extends {
  tag: Tag
  data: infer Data
}
  ? Data
  : never

export type UpdateTransactionData = UnionToData<FromElm, 'UpdateTransaction'>
