<Dialog bind:isVisible>
	<form novalidate on:submit|preventDefault="{doVerify}">
		<SInput label="Is Expense" type="checkbox" bind:field="{form.isExpense}" />
		<SInput label="Date" type="date" bind:field="{form.date}" />
		<SInput label="Category" bind:field="{form.category}" />
		<SInput label="Name" bind:field="{form.name}" />
		<SInput label="Price" bind:field="{form.price}" />
		<SInput label="Amount" bind:field="{form.amount}" />
		<SInput label="Description" bind:field="{form.description}" />
		<SInput label="Currency" bind:field="{form.currency}" />
		<!-- <div>{priceToShow}</div> -->
		<button class="button">Save</button>
	</form>
</Dialog>

<script lang="ts">
	import { v4 as uuidv4 } from 'uuid';
	import Dialog from './dialog.svelte'
	import validate from '../validation/validate'
	import { getFormValue, verify } from '../validation/lib/validation'
	import SInput from './s-input.svelte'
	import { required } from '../validation/lib/validators'
	import type { Field } from '../validation/lib/types'
	// import { decimals } from '../stores/transactions'
	import { getPriceToShow } from '../lib/transaction'
	import { sendToAllPeers } from '../lib/socket'
	import { key } from '../stores/credentials'
	import { iv, textEncoder } from '../crypto'

	export let id: undefined | string
	export let isVisible: boolean

	$: isNewTransaction = !!id

	type Form = {
		isExpense: Field
		date: Field
		category: Field
		name: Field
		price: Field
		amount: Field
		description: Field
		currency: Field
	}

	let form: Form = {
		isExpense: {
			value: '',
		},
		date: {
			value: '',
			validators: [required],
		},
		category: {
			value: '',
			validators: [required],
		},
		name: {
			value: '',
			validators: [required],
		},
		price: {
			value: '',
			validators: [required],
		},
		amount: {
			value: '',
			validators: [],
		},
		description: {
			value: '',
		},
		currency: {
			value: '',
			validators: [required],
		},
	}

	const update = (v: Form) => (form = v)
	$: validate(form, update)

	let isValidationInProgress = false
	$: isValidationInProgress = verify({
		success: async () => {
			if (!iv) {
				return
			}
			if (isNewTransaction) {
				const uuid = uuidv4()
			}
			const encoded = textEncoder.encode(
				JSON.stringify({ data: [getFormValue(form)] }),
			)
			console.log(await $key)
			const encryptedText = await crypto.subtle.encrypt(
				{
					name: 'AES-GCM',
					iv,
				},
				await $key,
				encoded,
			)

			sendToAllPeers(new Uint8Array([...iv, ...new Uint8Array(encryptedText)]))
		},
		isValidationInProgress,
		update,
		form,
	})

	const doVerify = () => {
		isValidationInProgress = true
	}

	// $: priceToShow = getPriceToShow(
	// 	getFormValue(form),
	// 	$decimals[form.currency.value],
	// )
</script>

<style lang="postcss">
	form {
		width: 100%;
		max-width: var(--max-page-width);
		padding: var(--space-lg);
		background-color: var(--c-gray-1);

		@media (--t) {
			width: var(--max-page-width);
		}

		& > :global(*:not(:first-child)) {
			margin-top: var(--space-sm);
		}

		& > :global(*:last-child) {
			margin-top: var(--space-md);
		}
	}
</style>
