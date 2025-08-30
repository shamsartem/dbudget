import { assertType } from 'vitest'

test('my types work properly', () => {
  // @ts-expect-error name is a string
  assertType(mount({ name: 42 }))
})
