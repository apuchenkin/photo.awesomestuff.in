const actionLifecyclesMiddleware = () => (next) => {
  const pending = {};

  return (action) => {
    const { lifecycle } = action;
    let ret;

    if (lifecycle) {
      ret = new Promise((resolve, reject) => {
        pending[lifecycle.resolve] = resolve;
        pending[lifecycle.reject] = reject;
      });
      next(action);
    } else {
      ret = next(action);
    }

    if (pending[action.type]) {
      const resolveOrReject = pending[action.type];
      delete pending[action.type];
      resolveOrReject(action);
    }

    return ret;
  };
};

export default actionLifecyclesMiddleware;
