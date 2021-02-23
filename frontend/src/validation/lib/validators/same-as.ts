import type { Form, Validator } from '../types'
import { functionCast, stringCast } from '../helpers'

export default (
	equalTo: ((outerF: Form, f: Form) => void) | string,
): Validator => ({
	name: 'sameAs',
	getErrorMessage: () => '',
	validator: (value) => {
		// const equalToFunc = functionCast(equalTo)
		// if (equalToFunc === null) {
		//   const inputName = stringCast(equalTo)
		//   if (inputName !== null) {
		//     return [value === f[inputName].val, f[inputName]]
		//   }
		// } else {
		//   return [value === equalToFunc(outerF, f).val, equalToFunc(outerF, f)]
		// }
		return true
	},
})
