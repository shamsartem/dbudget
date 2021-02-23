<form novalidate on:submit|preventDefault="{doVerify}">
	<SInput label="Username" bind:field="{form.username}" />
	<SInput label="Pasword" bind:field="{form.password}" />
	<button class="button">Log in</button>
</form>

<script lang="ts">
	import validate from '../validation/validate'
	import { getFormValue, verify } from '../validation/lib/validation'
	import SInput from '../components/s-input.svelte'
	import { required } from '../validation/lib/validators'
	import type { Field } from '../validation/lib/types'
	import { credentials } from '../stores/credentials'
	import { textEncoder } from '../crypto'

	type Form = {
		username: Field
		password: Field
	}

	let form: Form = {
		username: {
			value: '',
			validators: [required],
		},
		password: {
			value: '',
			validators: [required],
		},
	}

	const update = (v: Form) => (form = v)
	$: validate(form, update)

	let isValidationInProgress = false
	$: isValidationInProgress = verify({
		success: async () => {
			const { username, password } = getFormValue(form)
			const passwordHash = new Uint8Array(
				await crypto.subtle.digest('SHA-256', textEncoder.encode(password)),
			)
			credentials.set({ username, passwordHash })
		},
		isValidationInProgress,
		update,
		form,
	})

	const doVerify = () => {
		isValidationInProgress = true
	}
</script>

<style lang="postcss">
	form {
		flex-grow: 1;
		flex-direction: column;
		justify-content: center;
		display: flex;
	}
</style>
