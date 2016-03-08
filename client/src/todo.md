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
>> bug swithcin to ru on non-existent ru gallery
>> gallery style catch
>> bug with fast switching
>> bug with early loader hide
>> photo positioning is wrong

home:
- galley image fetch exact size
gallery:
- photo groups functionality
- refactor bricks, and improve algorithm
photo:
- styling
- arrows styles and hover/click effects
static:
- valid HTML
404:
- valid HTML

- transition effects
- check all smart forwards/redirects on 404s
- final optimization

-- raise bug about tuple/obj comparsion in html.lazy (tules are always unequal)
-- raise bug about structure re-rendering when first node is changes

-- problem with trailing slash "/ru"
-- add cached router views "do not re-render rendered handlers"
-- expose address through router insted of export

===== post ====
- loader effects
