{#if isVisible}
	<div
		class="dialog"
		use:portal
		transition:fade="{{ duration: BACKDROP_TRANSITION_DURATION }}"
		bind:this="{dialogEl}"
		on:keydown="{handleKeyDown}"
	>
		<div
			class="dialogWrapper"
			class:isFullscreen
			use:clickoutside
			on:clickoutside="{hideDialog}"
		>
			<button class="closeButton" on:click="{hideDialog}">
				<span class="visuallyHidden">Close dialog</span>
			</button>
			<slot />
		</div>
	</div>
{/if}

<script lang="ts">
	// please remove the following comment when working with this file
	// @ts-nocheck
	import { fade } from 'svelte/transition'
	import { createFocusTrap } from 'focus-trap'
	import portal from '../actions/portal'
	import clickoutside from '../actions/clickoutside'
	import isBackdropVisible from '../stores/is-backdrop-visible'
	import { BACKDROP_TRANSITION_DURATION, KEY_ESC } from '../const'

	export let isVisible: boolean
	export let isFullscreen = false

	let dialogEl: HTMLDivElement | undefined
	let focusTrap

	const activateFocusTrap = () => {
		focusTrap = createFocusTrap(dialogEl, {
			onDeactivate() {
				isVisible = false
			},
		})
		focusTrap.activate()
	}

	const deactivateFocusTrap = () => {
		focusTrap.deactivate()
	}

	let firstTime = true
	$: if (firstTime) {
		firstTime = false
	} else if (isVisible) {
		isBackdropVisible.set(true)
		setTimeout(() => {
			activateFocusTrap()
		})
	} else {
		deactivateFocusTrap()
		isBackdropVisible.set(false)
	}

	const hideDialog = () => {
		isVisible = false
	}

	const handleKeyDown = (
		e: KeyboardEvent & {
			target: EventTarget & HTMLDivElement
		},
	) => {
		e.code === KEY_ESC && hideDialog()
	}
</script>

<style lang="postcss">
	.dialog {
		display: flex;
		position: fixed;
		z-index: var(--z-dialog);
		top: 0;
		right: 0;
		bottom: 0;
		left: 0;
		align-items: center;
		justify-content: center;
		width: 100%;
		height: 100%;
		overflow-y: scroll;
	}

	.dialogWrapper {
		display: flex;
		flex-direction: column;
		align-items: center;
		width: 100%;

		@media (--t) {
			width: auto;
		}

		&.isFullscreen {
			width: 100%;
		}
	}

	.closeButton {
		display: flex;
		position: absolute;
		z-index: var(--z-default);
		top: 10px;
		right: 10px;
		align-items: center;
		justify-content: center;
		width: 40px;
		height: 40px;
		border-radius: 50%;
		background-color: var(--c-black);

		&::before,
		&::after {
			content: '';
			position: absolute;
			width: 60%;
			height: 2px;
			transform: rotate(45deg);
			background-color: var(--c-gray-8);
		}

		&::after {
			transform: rotate(-45deg);
		}
	}
</style>
