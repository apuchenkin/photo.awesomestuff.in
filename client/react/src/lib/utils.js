export default {
  pick(object, params) {
  	return Object.keys(object).filter(k => params.indexOf(k) >= 0).reduce((o,k) => {o[k] = object[k]; return o}, {})
  }
}
