const sveltePreprocess = require('svelte-preprocess')
const fs = require('fs')

const prependData = ['media-queries'].reduce((acc, fileName) => {
	const fileContent = fs
		.readFileSync(`./src/styles/${fileName}.pcss`)
		.toString()
	return `${acc}${fileContent}`
}, '')

module.exports = {
	preprocess: sveltePreprocess({
		postcss: {
			prependData,
			plugins: [
				require('postcss-nested'),
				require('postcss-preset-env')({ stage: 0 }),
			],
		},
	}),
}
