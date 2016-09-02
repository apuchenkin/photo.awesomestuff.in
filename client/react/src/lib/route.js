export default class Route extends Object {
  constructor(obj) {
    super(obj);

    const
      me = this,
      wrappedResolve = (promise) => {
        return (location) => {
          // cmp && cmp.setState({
          //   isLoading: true
          // });
          // debugger;
          return promise(location)
            .then(data => {
              Object.assign(me.state, data);
              // cmp && cmp.setState({
              //   isLoading: false
              // });
              return data;
            });
        };
      };

    Object.assign(me, {
      resolve: wrappedResolve(obj.resolve)
    });
  }
}
