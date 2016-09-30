import memoize from 'memoizee';
import { startLoading, stopLoading } from '../actions/loader';

export default (props) => {
  const
    { store, resolve } = props,
    resolve$ = memoize(resolve.bind(props), { promise: 'then' }),
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
    resolve: memoize(wrappedResolve(resolve$).bind(props), { promise: 'then' }),
  });
};
