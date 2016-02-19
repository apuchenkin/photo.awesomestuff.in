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
>> Less magic -> better perfomance and caching (remove update router and whole state from router)
>> bug route with params
>> configs??
>> refactor after bugfix
>> config cache on/off
>> general refactoring/simplification
>> static routes

- move to separate repo
- complete/fix tests
- private / public contexts types
- drop unneeded deps on libs

--- app ---
- port logic
- valid HTML
- effects and post libs
- styles

--- post ---
- inject parent to speed up??
- (Bug) replacePath instead setPath on fallback (404)
- (Improvement) greedy matcher config (matches first found route or latest one)
- (Improvement) trailing slash route?
- (Improvement) perfomance

-- raise bug about tuple/obj comparsion in html.lazy (tules are always unequal)
-- raise bug about structure re-rendering when first node is changes

-- maybe Html -> dict Html + general layout
