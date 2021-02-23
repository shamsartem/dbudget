import type { Validator } from '../types'

// "required" core, used in almost every validator to allow empty values
export const req = (value: any): boolean => {
	if (Array.isArray(value)) return value.length > 0
	if (value === undefined || value === null) {
		return false
	}

	if (value === false) {
		return true
	}

	if (value instanceof Date) {
		// invalid date won't pass
		return !isNaN(value.getTime())
	}

	if (typeof value === 'object') {
		for (let _ in value) return true
		return false
	}

	return String(value).length > 0
}

// get length in type-agnostic way
export const len = (value: unknown): number => {
	if (Array.isArray(value)) return value.length
	if (typeof value === 'object') {
		return Object.keys(value).length
	}
	return String(value).length
}

export const ref = (reference, f) =>
	typeof reference === 'function' ? reference.call(f) : f[reference]

export const createRegExpValidator = (
	regExp: RegExp,
	rest: Omit<Validator, 'validator'>,
): Validator => ({
	validator: (value) => !req(value) || regExp.test(value),
	...rest,
})

export const isDateOrStringThatCanBeConvertedToNumber = (val: any): boolean =>
	val instanceof Date || (typeof val === 'string' && !isNaN(+val))
