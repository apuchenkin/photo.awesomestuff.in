import { START_LOADING, STOP_LOADING } from '../actions/loader';

const initialState = {
  count: 0,
};

export default function loading(state = initialState, action) {
  switch (action.type) {
    case START_LOADING:
      return { count: state.count + 1 };
    case STOP_LOADING:
      return { count: state.count - 1 > 0 ? state.count - 1 : 0 };
    default:
      return state;
  }
}
