export default class Route extends Object {
  constructor(obj) {
    super(obj);

    const
      me = this,
      wrappedResolve = promise => location => promise(location)
        .then((data) => {
          Object.assign(me.state, data);
          return data;
        });

    Object.assign(me, {
      resolve: wrappedResolve(obj.resolve),
    });
  }
}
