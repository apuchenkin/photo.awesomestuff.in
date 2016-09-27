import { startLoading, stopLoading } from '../actions/loader';

export default (props) => {
  const
    { store } = props,
    wrappedResolve = promise => (location) => {
      store.dispatch(startLoading());
      return promise(location)
        .then((data) => {
          Object.assign(props.state, data);
          store.dispatch(stopLoading());
          return data;
        });
    };

  return Object.assign(props, {
    resolve: wrappedResolve(props.resolve),
  });
};
