<label
	for="{forInputWithId}"
	class:visuallyHidden="{hasPlaceholder}"
	class:checkbox="{isCheckbox}"
>
	{#if required}
		<i aria-hidden="true" title="required">*</i>
		<i class="visuallyHidden">required</i>
	{/if}
	{label}
</label>

<script lang="ts">
	export let forInputWithId: string
	export let hasPlaceholder: boolean
	export let required: boolean
	export let label: string
	export let isCheckbox: boolean = false
</script>

<style lang="postcss">
	label {
		position: relative;
		max-width: 100%;
		margin-bottom: 3px;
		line-height: var(--lh-sm);

		&.checkbox {
			--before-size: 20px;

			padding-left: calc(var(--before-size) + 10px);

			&::before,
			&::after {
				--size: var(--before-size);

				content: '';
				position: absolute;
				top: 0;
				left: 0;
				width: var(--size);
				height: var(--size);
			}

			&::before {
				transition: border-color 0.3s ease-in-out;
				border: 1px solid var(--c-gray-4);
				border-radius: var(--bdrs-md);
			}

			&:hover::before {
				border-color: var(--c-gray-6);
			}

			&::after {
				--size: 12px;
				--size-diff: calc(var(--before-size) - var(--size));
				--offset: calc(var(--size-diff) / 2);

				top: var(--offset);
				left: var(--offset);
				border-radius: var(--bdrs-sm);
				opacity: 0;
				background-color: var(--c-gray-5);
			}
		}
	}
</style>
