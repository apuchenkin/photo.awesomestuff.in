import fetch from 'node-fetch';
import * as R from 'ramda';

const host = 'http://photo.awesomestuff.in';
const basename = '/api/v1';
const Authorization = process.env.AUTH;

const options = {
  method: 'GET',
  headers: {
    "Cache-Control": 'no-cache',
    Authorization,
  },
};


export default async () => {
  const pages = await fetch(`${host}${basename}/page`, options).then(res => res.json());

  const pages$ = await Promise.all(
    pages.map((page: any) => fetch(
      `${host}${basename}/page/${page.alias}`,
      options,
    )
    .then(res => res.json())
    )
  );

  const categories = await fetch(`${host}${basename}/category`, options).then(res => res.json());

  const categories$ = await Promise.all(
    categories.map((category: any) => fetch(
      `${host}${basename}/category/${category.name}`,
      options,
    )
    .then(res => res.json())
    .catch(e=> {
      console.log(e);
    })
    )
  );

  const photos = await Promise.all(
    categories.map((category: any) => fetch(
      `${host}${basename}/category/${category.name}/photo`,
      options,
    )
    .then(res => res.json())
    .catch(() => [])
    .then(photos => photos.map((photo: any) => ({
      ...photo,
      category: category.name,
    })))
    )
  );

  const photos$ = R.pipe(
    R.unnest,
    R.groupBy(R.prop('id')),
    // @ts-ignore
    R.map((photoList: Photo[]) => ({
      ...R.head(photoList),
      category: R.map(R.prop('category'), photoList)
    })),
    R.values,
  )(photos);

  return {
    pages: pages$.filter(Boolean),
    categories: categories$.filter(Boolean),
    photos: photos$.filter(Boolean),
  } as any;
};
