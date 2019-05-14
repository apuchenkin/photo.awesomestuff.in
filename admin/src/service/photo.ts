import Service from './api';

export const getThumb = (size: number, path: string): string => {
  const chunks = path.split('/');
  const last = chunks.pop();

  if (last) {
    const [a, b] = last.split('.');

    chunks.push(`${a}@${size}.${b}`);
  }

  return ['/static', 'thumb', ...chunks].join('/');
}

export default class PhotoService extends Service {
  fetchPhoto = (photoId: number) => {
    return this.fetch(`/photo/${photoId}`).then(photo => ({
      id: photoId,
      ...photo,
    }));
  }
}
