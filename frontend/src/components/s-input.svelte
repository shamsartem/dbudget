<div class="wrapper">
	{#if type !== 'checkbox'}
		<SInputLabel
			forInputWithId="{id}"
			hasPlaceholder="{hasPlaceholder}"
			label="{label}"
			required="{required}"
		/>
	{/if}
	<input
		type="{type}"
		on:input="{handleInput}"
		on:blur="{handleBlur}"
		id="{id}"
		placeholder="{placeholder}"
		required="{required}"
		class:isHighlighted
		class:hasErrors
		class:visuallyHidden="{type === 'checkbox'}"
		bind:this="{el}"
		aria-describedby="{errorId}"
		{...$$restProps}
	/>
	{#if type === 'checkbox'}
		<SInputLabel
			isCheckbox="{true}"
			forInputWithId="{id}"
			hasPlaceholder="{hasPlaceholder}"
			label="{label}"
			required="{required}"
		/>
	{/if}
	<div class="error" id="{errorId}">
		{#each errorMessages as message}<span>{message}</span>{/each}
	</div>
</div>

<script lang="ts">
	import { onMount } from 'svelte'
	import randomId from '../utils/random-id'
	import type { Field } from '../validation/lib/types'
	import SInputLabel from './s-input-label.svelte'

	export let field: Field
	export let label: string
	export let hasPlaceholder = false
	export let isHighlighted = false
	export let errorId = randomId('inputEr')
	export let id = randomId('input')
	export let type: 'date' | 'checkbox' | 'text' = 'text'

	let el: HTMLInputElement

	$: errorMessages =
		typeof field !== 'string' &&
		field.errorMessages &&
		field.errorMessages.length &&
		field.dirty
			? field.errorMessages
					.reduce((messages, message) => {
						if (message) {
							messages.push(message)
						}
						return messages
					}, [])
					.map((message, i) => (i ? `, ${message}` : message))
			: []

	$: hasErrors = errorMessages.length > 0

	$: required = field.validators
		? field.validators.some((validator) => validator.name === 'required')
		: undefined

	const placeholder = hasPlaceholder ? label : null

	const handleInput = (e) => {
		field.value = type.match(/^(number|range)$/)
			? +e.target.value
			: e.target.value
	}

	function handleBlur() {
		if (!field.dirty) {
			field.dirty = true
		}
	}

	onMount(() => {
		field.el = el
	})
</script>

<style lang="postcss">
	.wrapper {
		display: flex;
		flex-direction: column;
	}

	input[type='text'],
	input[type='search'],
	input[type='date'] {
		height: 30px;
		padding-left: 7px;
		border: 1px solid var(--c-gray-6);
		border-radius: var(--bdrs-md);
		outline: none;
		background-color: var(--c-gray-2);
		color: var(--c-gray-9);

		&:focus {
			border-color: var(--c-gray-9);
		}

		&.isHighlighted {
			background-color: var(--c-warning-dark);
		}

		&.hasErrors {
			border-color: var(--c-danger);
		}

		&::placeholder {
			color: var(--c-gray-5);
		}
	}

	input[type='checkbox'] {
		&:focus + :global(label::before) {
			outline: 1px solid var(--c-gray-9);
		}

		&:checked + :global(label::after) {
			opacity: 1;
		}
	}

	.error {
		color: var(--c-danger);
	}
</style>
