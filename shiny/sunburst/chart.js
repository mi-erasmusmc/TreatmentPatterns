/*

Copyright 2017 Observational Health Data Sciences and Informatics

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Author: Alexander Saltykov

*/

define(["d3", "lodash", "d3-tip"],
	function(d3, lodash, d3tip) {
	"use strict";

	class Chart {
	  static get chartTypes() {
	    return {
	      AREA: 'AREA',
	      BOXPLOT: 'BOXPLOT',
	      DONUT: 'DONUT',
	      HISTOGRAM: 'HISTOGRAM',
	      LINE: 'LINE',
	      TRELLISLINE: 'TRELLISLINE',
	    };
	  }

    render(data, target, w, h, chartOptions) {

			if (typeof target == "string") {
				target = document.querySelector(target);
			}

			if (!this.doResize) {
				this.doResize = lodash.debounce(() => {
					this.render(data, target,target.clientWidth,target.clientHeight,chartOptions);
				}, 250);
				window.addEventListener("resize", this.doResize);
			}

		}

	  getOptions(chartSpecificDefaults, customOptions) {
	    const options = Object.assign({}, {
		      margins: {
		        top: 10,
		        right: 10,
		        bottom: 10,
		        left: 10,
		      },
		      xFormat: d3.format(',.0f'),
		      yFormat: d3.format('s'),
		      colors: d3.scaleOrdinal(d3.schemeCategory20.concat(d3.schemeCategory20)),
		    },
		    // clone objects
			  Object.assign({}, chartSpecificDefaults),
			  Object.assign({}, customOptions)

		  );
		  
		  return options;
	  }

	  createSvg(target, width, height) {

	    this.destroyTipIfExists();

	    const container = d3.select(target);
	    container.select('svg').remove();
	    const chart = container.append('svg')
	      .attr('preserveAspectRatio', 'xMinYMin meet')
	      .attr('viewBox', `
	        0
	        0
	        ${width}
	        ${height}`)
	      .append('g')
	      .attr('class', 'chart');

			this.chart = chart;

	    return chart;
	  }

	  useTip(tooltipConfigurer = () => {}, options) {

	    this.destroyTipIfExists();

	    this.tip = d3tip()
	      .attr('class', 'd3-tip');

	    tooltipConfigurer(this.tip, options);

	    if (this.chart) {
	      this.chart.call(this.tip);
	    }

	    return this.tip;
	  }

	  destroyTipIfExists() {
	    if (this.tip) {
	      this.tip.destroy();
	    }
	  }


	  static normalizeDataframe(dataframe) {
	    // rjson serializes dataframes with 1 row as single element properties.
	    // This function ensures fields are always arrays.
	    const keys = d3.keys(dataframe);
	    const frame = Object.assign({}, dataframe);
	    keys.forEach((key) => {
	      if (!(dataframe[key] instanceof Array)) {
	        frame[key] = [dataframe[key]];
	      }
	    });
	    return frame;
	  }

	  static dataframeToArray(dataframe) {
	    // dataframes from R serialize into an obect where each column is an array of values.
	    const keys = d3.keys(dataframe);
	    let result;
	    if (dataframe[keys[0]] instanceof Array) {
	      result = dataframe[keys[0]].map((d, i) => {
	        const item = {};
	        keys.forEach(p => {
	          item[p] = dataframe[p][i];
	        });
	        return item;
	      });
	    } else {
	      result = [dataframe];
	    }
	    return result;
	  }

	  get formatters() {
	    return {
	      formatSI: (p) => {
	        p = p || 0;
	        return (d) => {
	          if (d < 1) {
	            return Math.round(d, p);
	          }
	          const prefix = d3.format(',.0s', d);
	          return prefix(d);
	        }
	      },
	    };
	  }

	  truncate(text, width) {
	    text.each(function() {
	      const t = d3.select(this);
	      const originalText = t.text();
	      let textLength = t.node().getComputedTextLength();
	      let text = t.text();
	      while (textLength > width && text.length > 0) {
	        text = text.slice(0, -1);
	        t.text(`${text}...`);
	        textLength = t.node().getComputedTextLength();
	      }
	      t.append('title').text(originalText);
	    });
	  }

	  wrap(text, width, truncateAtLine) {
	    text.each(function () {
	    const text = d3.select(this);
	    const fullText = text.text();
	      const words = text.text().split(/\s+/).reverse();
	      let line = [];
	      let word;
	      let lineNumber = 0;
	      let lineCount = 0;
	      const lineHeight = 1.1; // ems
	      const y = text.attr('y');
	      const dy = parseFloat(text.attr('dy'));
	      let tspan = text
	        .text(null)
	        .append('tspan')
	        .attr('x', 0)
	        .attr('y', y)
	        .attr('dy', `${dy}em`);
	      while (word = words.pop()) {
	        line.push(word);
	        tspan.text(line.join(' '));
	        if (tspan.node().getComputedTextLength() > width) {
	          if (line.length > 1) {
	            line.pop(); // remove word from line
	            words.push(word); // put the word back on the stack
	            const text = !!truncateAtLine && ++lineCount === truncateAtLine ? `${line.splice(0, line.length - 1).join(' ')}...` : line.join(' ');
	            tspan.text(text);
	          }
	          line = [];
	          tspan = text
	            .append('tspan')
	            .attr('x', 0)
	            .attr('y', y)
	            .attr('dy', `${++lineNumber * lineHeight + dy}em`);
	          if (!!truncateAtLine && truncateAtLine === lineCount) {
	            tspan.remove();
	            break;
	          }
	        }
	      }
	      text.append('title').text(fullText);
	    });
	  }

	  // Tooltips

	  tooltipFactory(tooltips) {
	    return (d) => {
	      let tipText = '';

	      if (tooltips !== undefined) {
	        for (let i = 0; i < tooltips.length; i = i + 1) {
	          let value = tooltips[i].accessor(d);
	          if (tooltips[i].format !== undefined) {
	            value = tooltips[i].format(value);
	          }
	          tipText += `${tooltips[i].label}: ${value}</br>`;
	        }
	      }

	      return tipText;
	    };
	  }

	  lineDefaultTooltip(
	    xLabel,
	    xFormat,
	    xAccessor,
	    yLabel,
	    yFormat,
	    yAccessor,
	    seriesAccessor
	  ) {
	    return (d) => {
	      let tipText = '';
	      if (seriesAccessor(d))
	        tipText = `Series: ${seriesAccessor(d)}</br>`;
	      tipText += `${xLabel}: ${xFormat(xAccessor(d))}</br>`;
	      tipText += `${yLabel}: ${yFormat(yAccessor(d))}`;
	      return tipText;
	    }
	  }

	  donutDefaultTooltip(labelAccessor, valueAccessor, percentageAccessor) {
	    return (d) =>
	      `${labelAccessor(d)}: ${valueAccessor(d)} (${percentageAccessor(d)})`
	  }

	  static mapMonthYearDataToSeries(data, customOptions) {
	    const defaults = {
	      dateField: 'x',
	      yValue: 'y',
	      yPercent: 'p'
	    };

	    const options = Object.assign({},
	      defaults,
	      customOptions
	    );

	    const series = {};
	    series.name = 'All Time';
	    series.values = [];
	    data[options.dateField].map((datum, i) => {
	      series.values.push({
	        xValue: new Date(Math.floor(data[options.dateField][i] / 100), (data[options.dateField][i] % 100) - 1, 1),
	        yValue: data[options.yValue][i],
	        yPercent: data[options.yPercent][i]
	      });
	    });
	    series.values.sort((a, b) => a.xValue - b.xValue);

	    return [series]; // return series wrapped in an array
	  }

	  static prepareData(rawData, chartType) {
	    switch (chartType) {
	      case this.chartTypes.BOXPLOT:
	        if (!rawData.CATEGORY.length) {
		        return null;
		      }
		      const data = rawData.CATEGORY.map((d,i) => ({
		        Category: rawData.CATEGORY[i],
		        min: rawData.MIN_VALUE[i],
		        max: rawData.MAX_VALUE[i],
		        median: rawData.MEDIAN_VALUE[i],
		        LIF: rawData.P10_VALUE[i],
		        q1: rawData.P25_VALUE[i],
		        q3: rawData.P75_VALUE[i],
		        UIF: rawData.P90_VALUE[i],
		      }), rawData);
		      const values = Object.values(data);
		      const flattenData = values.reduce((accumulator, currentValue) =>
		      		accumulator.concat(currentValue),
		      		[]
		      	);
		      if (!flattenData.length) {
		        return null;
		      }

		      return data;
	    }
	  }

		dispose() {
			this.destroyTipIfExists();
			if (this.doResize) {
				window.removeEventListener("resize", this.doResize);
			}
		}

	}

	return Chart;
});
