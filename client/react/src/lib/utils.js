import Promise from 'promise';

export default {
  pick(object, params) {
  	return Object.keys(object).filter(k => params.indexOf(k) >= 0).reduce((o,k) => {o[k] = object[k]; return o}, {})
  },

  fetchAll(object) {
    const keys = Object.keys(object);

    return Promise.all(keys.map(k => object[k])).then(data => {
        return keys.reduce((acc, k, idx) => {acc[k] = data[idx]; return acc}, {});
      });
  },

  // Returns a function, that, as long as it continues to be invoked, will not
  // be triggered. The function will be called after it stops being called for
  // N milliseconds. If `immediate` is passed, trigger the function on the
  // leading edge, instead of the trailing.
  debounce(func, wait, immediate) {
  	var timeout;

  	return function() {
  		var context = this, args = arguments;
  		var later = function() {
  			timeout = null;
  			if (!immediate) func.apply(context, args);
  		};
  		var callNow = immediate && !timeout;
  		clearTimeout(timeout);
  		timeout = setTimeout(later, wait);
  		if (callNow) func.apply(context, args);
  	};
  }
}
