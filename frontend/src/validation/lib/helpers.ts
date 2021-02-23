import isFunction from 'lodash/isFunction'
import isObject from 'lodash/isObject'
import isString from 'lodash/isString'
import isBoolean from 'lodash/isBoolean'

const createCastFunction = <T>(checker: (any: any) => boolean) => (
	val: any,
): T | null => (checker(val) ? (val as T) : null)

export const functionCast = createCastFunction<Function>(isFunction)
export const objectCast = createCastFunction<object>(isObject)
export const stringCast = createCastFunction<string>(isString)

export const promiseCast = (val: any): null | Promise<any> => {
	if (objectCast(val) !== null) {
		const { then } = val as { then: any }
		if (functionCast(then) !== null) {
			return val as Promise<any>
		}
	}

	return null
}

export const booleanCast = (val: any): null | boolean => {
	if (isBoolean(val)) {
		return val as boolean
	}

	return null
}
