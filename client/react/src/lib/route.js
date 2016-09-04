export default class Route extends Object {
  constructor(obj) {
    super(obj);

    const
      me = this,
      wrappedResolve = (promise) => {
        return (location) => {
          const cmp = location.routes && location.routes[0].cmp;

          cmp && cmp.setState({
            isLoading: true
          });

          return promise(location)
            .then(data => {
              Object.assign(me.state, data);
              cmp && cmp.setState({
                isLoading: false
              });
              return data;
            });
        };
      };

    Object.assign(me, {
      resolve: wrappedResolve(obj.resolve)
    });
  }
}
