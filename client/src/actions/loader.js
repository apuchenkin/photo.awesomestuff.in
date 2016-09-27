/*
 * action types
 */
export const START_LOADING = 'START_LOADING';
export const STOP_LOADING = 'STOP_LOADING';

/*
 * action creators
 */
export function startLoading() {
  console.log('startLoading');
  return { type: START_LOADING };
}

export function stopLoading() {
  console.log('stopLoading');
  return { type: STOP_LOADING };
}
