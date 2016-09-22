export default (props) => {
  const
    wrappedResolve = promise => location => promise(location)
      .then((data) => {
        Object.assign(props.state, data);
        return data;
      });

  return Object.assign(props, {
    resolve: wrappedResolve(props.resolve),
  });
};
