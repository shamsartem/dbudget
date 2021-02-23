import cloneDeep from 'lodash/cloneDeep'
import type { Validator, Form, VerifyArg } from './types'
import { promiseCast } from './helpers'

export const createValidateFunction = () => {
	let initPrevForm: Form = {}
	let outerForm: Form

	const validateOnlyChanged = (
		form: Form,
		update: (v: Form) => void,
		prevForm: Form = initPrevForm,
		outer = true,
	) => {
		if (outer) {
			outerForm = form
		}
		Object.keys(form).forEach((key) => {
			if (!('value' in form[key])) {
				const innerForm = form[key]
				if (!prevForm[key]) {
					prevForm[key] = {}
					if (Array.isArray(innerForm)) {
						prevForm[key] = []
					}
				}

				validateOnlyChanged(
					innerForm,
					(value) => {
						form[key] = value
						update(form)
					},
					prevForm[key],
					false,
				)

				return
			}

			if (!form[key].validators?.length) {
				return
			}

			if (form[key].value !== prevForm[key]?.value) {
				prevForm[key] = {}
				prevForm[key].value = form[key].value
				form[key].isValidArray = new Array(form[key].validators.length).fill(
					null,
				)
				form[key].errorMessages = new Array(form[key].validators.length).fill(
					'',
				)
				update(form)
				form[key].validators.forEach((validator: Validator, i: number) => {
					const setErrorMessage = (isValid: boolean) => {
						form[key].isValidArray[i] = isValid
						form[key].errorMessages[i] = isValid
							? ''
							: validator.getErrorMessage()
					}
					const isValidOrPromise = validator.validator(form[key].value)
					form[key].isValidArray[i] = isValidOrPromise
					const validationPromise = promiseCast(isValidOrPromise)
					if (validationPromise === null) {
						setErrorMessage(isValidOrPromise as boolean)
					} else {
						validationPromise.then((isValid: boolean) => {
							if (form[key].isValidArray[i] === validationPromise) {
								setErrorMessage(isValid)
								update(form)
							}
						})
					}
				})
				update(form)
			}
		})
	}
	return validateOnlyChanged
}

const getFieldValidity = (
	arr: Array<boolean | Promise<boolean>>,
): null | boolean => {
	if (arr.some((el) => el !== true && el !== false)) {
		return null
	}

	if (arr.some((el) => el === false)) {
		return false
	}

	return true
}

export const verify = (arg: VerifyArg) => {
	const isOuter = !!arg.success

	if (isOuter) {
		arg.domEls = []
	}

	let formValidity: boolean | null = true

	const setFormValidity = (fieldValidity: boolean | null) => {
		if (fieldValidity === null) {
			formValidity = null
		} else if (fieldValidity === false && formValidity === true) {
			formValidity = false
		}
	}

	if (arg.isValidationInProgress) {
		Object.keys(arg.form).forEach((key) => {
			if (!('value' in arg.form[key])) {
				let innerArgs = cloneDeep(arg)
				delete innerArgs.domEls
				innerArgs = {
					...arg,
					...innerArgs,
				}
				delete innerArgs.success
				let innerValidity: boolean
				innerArgs.update = (value) => {
					arg.form[key] = value
					arg.update(arg.form)
				}
				innerArgs.setInnerValidity = (validity) => {
					innerValidity = validity
				}
				innerArgs.form = innerArgs.form[key]
				verify(innerArgs)
				setFormValidity(innerValidity)
			} else if (arg.form[key].validators?.length) {
				arg.form[key].dirty = true
				let fieldValidity = getFieldValidity(arg.form[key].isValidArray)
				setFormValidity(fieldValidity)
				if (
					arg.isFocusDisabled !== true &&
					arg.form[key].el &&
					!fieldValidity
				) {
					arg.domEls.push(arg.form[key].el)
				}
			}
		})

		if (arg.update) {
			arg.update(arg.form)
		}

		if (arg.setInnerValidity) {
			arg.setInnerValidity(formValidity)
		}

		if (formValidity !== null) {
			if (formValidity && isOuter) {
				arg.success()
			} else if (isOuter && arg.domEls.length) {
				const firstInputElement = document.querySelector(
					arg.domEls.reduce(
						(selector, el, i) =>
							i === 0 ? `#${el.id}` : `${selector}, #${el.id}`,
						'',
					),
				) as HTMLInputElement
				firstInputElement.focus()
			}
			return false
		}
		return true
	}
	return false
}

export const getFormValue = <T>(form: Form, outer = true): T => {
	let startingObj = {} as T
	if (outer && Array.isArray(form)) {
		startingObj = ([] as unknown) as T
	}

	return Object.keys(form).reduce((acc, key) => {
		acc[key] = 'value' in form[key] ? form[key].value : getFormValue(form[key])

		return acc
	}, startingObj)
}
