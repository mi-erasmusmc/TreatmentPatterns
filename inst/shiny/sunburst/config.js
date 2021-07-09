requirejs.config({
	packages: [
		{
			name: "sunburst",
			location: "../sunburst",
			main: "main"
		}	
  ],
	paths: {
    "jquery": "https://code.jquery.com/jquery-1.11.2.min",
		"numeral": "https://cdnjs.cloudflare.com/ajax/libs/numeral.js/2.0.6/numeral.min",
		"d3": "https://cdnjs.cloudflare.com/ajax/libs/d3/4.10.0/d3.min",
		"d3-tip": "https://cdnjs.cloudflare.com/ajax/libs/d3-tip/0.7.1/d3-tip.min",
		"lodash": "https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.11/lodash.min"
	},
});
