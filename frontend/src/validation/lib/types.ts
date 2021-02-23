export type GetErrorMessage = () => string

export type ErrorMessages = {
	[key: string]: GetErrorMessage
}

export type Validator = {
	validator: (value: any) => boolean | Promise<boolean>
	getErrorMessage: GetErrorMessage
	name?: string
}

export type Field = {
	value: string | number // value of the field
	el?: HTMLInputElement // input HTML element to put focus on in case of the error (you can turn this off but it's required for accessibility)
	errorMessages?: Array<string> // array of indexed error messages. If there is no error - you will see an empty string
	// related?: Array<Field> // array of fields that are related to the validation of the current field
	isValidArray?: Array<boolean | Promise<boolean>> // array of indexed values
	validators?: Array<Validator> // array of validator functions. They should only return true or false. You can use a function that returns promise for async validation
	dirty?: boolean // it is set to true if you try to verify the form. You can use it to display your error messages
}

export type Form =
	| {
			[key: string]: Field | Array<Field> | Form
	  }
	| Array<Field>

export type VerifyArg = {
	isValidationInProgress: boolean
	success: () => void
	update: (f: Form) => void
	form: Form
	domEls?: Array<HTMLInputElement>
	isFocusDisabled?: true
	setInnerValidity?: (validity: boolean) => void
}
