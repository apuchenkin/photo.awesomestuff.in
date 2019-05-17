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

  upload = (file: File, category: Category) => {
    const body = new FormData();
    body.append('file', file);

    return fetch(`${this.endpoint}/upload/${category.name}`, {
      method: 'POST',
      body,
    }).then(async res => {
      if (!res.ok) {
        const error = await res.text();
        throw error;
      }

      return res;
    });
  }
}
