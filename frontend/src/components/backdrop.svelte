<div
	class:visible
	style="transition-duration: {BACKDROP_TRANSITION_DURATION}ms;"
></div>

<script lang="ts">
	import isBackdropVisible from '../stores/is-backdrop-visible'
	import { BACKDROP_TRANSITION_DURATION } from '../const'

	let timeout: number
	let visible = false

	let firstTime = true
	$: if (firstTime) {
		firstTime = false
	} else if ($isBackdropVisible && !document.body.style.marginTop) {
		clearTimeout(timeout)

		const scrollY = window.scrollY || document.documentElement.scrollTop

		visible = true

		if (window.innerWidth > document.documentElement.clientWidth) {
			document.documentElement.classList.add('hasScrollbar')
		}

		document.body.style.marginTop = `-${scrollY}px`
		document.documentElement.classList.add('noScroll')
		document.body.classList.add('overflowHidden')

		visible = true
	} else if (!$isBackdropVisible) {
		timeout = setTimeout(() => {
			const newScrollTop = -document.body.style.marginTop.slice(0, -2)

			document.documentElement.classList.remove('hasScrollbar', 'noScroll')
			document.body.classList.remove('overflowHidden')
			document.body.style.marginTop = null
			window.scrollTo(null, newScrollTop)

			setTimeout(() => {
				visible = false
			}, BACKDROP_TRANSITION_DURATION)
		})
	}
</script>

<style lang="postcss">
	div {
		visibility: hidden;
		position: fixed;
		z-index: var(--z-backdrop);
		top: 0;
		right: 0;
		bottom: 0;
		left: 0;
		width: 100%;
		height: 100%;
		transition-property: opacity, visibility;
		opacity: 0;
		background-color: var(--c-gray-1-7);
		pointer-events: none;
	}

	div.visible {
		visibility: visible;
		opacity: 1;
		pointer-events: initial;
	}

	:global(html.noScroll) {
		height: 100%;
		overflow-y: hidden;
	}

	:global(html.hasScrollbar) {
		overflow-y: scroll;
	}

	:global(body.overflowHidden) {
		overflow: hidden;
	}
</style>
