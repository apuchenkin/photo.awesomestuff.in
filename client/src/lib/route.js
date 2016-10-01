import { Promise } from 'es6-promise';
import { startLoading, stopLoading } from '../actions/loader';

export default (props) => {
  const
    { store, actions } = props,
    resolve = (location) => {
      if (actions && actions.length) {
        const promises = actions
          .map(fn => fn(location))
          .filter(x => !!x)
          .map((action) => {
            store.dispatch(action);

            return action.payload;
          });

        if (promises.length) {
          store.dispatch(startLoading());
          return Promise.all(promises)
            .then(() => store.dispatch(stopLoading()));
        }
      }

      return Promise.resolve();
    };

  const onEnter = (location, replace, cb) => resolve(location)
    .catch(cb)
    .then(() => cb());

  return Object.assign(props,
    props.resolve ? {} : { resolve },
    props.onEnter ? {} : { onEnter },
  );
};
