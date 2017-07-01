export const SET_META = 'SET_META';

export function setMeta({ title, description, langs }) {
  return {
    type: SET_META,
    meta: {
      title,
      description,
      langs,
    },
  };
}
