>> move params to router
>> params dict
>> improve handlers chain
>> nested route parser
>> optional params
>> decentralize state to handlers: see (https://gist.github.com/evancz/2b2ba366cae1887fe621#focus)
>> router cache
>> how to bypass state to buildURL to simplify API?
>> html cache
>> getRoute and getParams expose to router public API
>> add route constraints (probably regex check)
>> fallback route on failed routematch
>> forwards
>> i18n
>> handlers cache
>> meta information ports

- Less magic -> better perfomance and caching (remove update router and whole state from router)
- config cache on/off
- drope unneeded deps on libs

- tests
- static routes
- general refactoring
- perfomance

- valid HTML
- styles
- effects

- (Bug) replacePath instead setPath on fallback (404)
- (Improvement) greedy matcher config (matches first found route or latest one)
- (Improvement) trailing slash route?
