import * as React from 'react';
import makeRouteConfig from 'found/lib/makeRouteConfig';
import Redirect from 'found/lib/Redirect';
import Route from './route';
import HttpError from 'found/lib/HttpError';

import {
  App,
  Home,
  Page,
  Category,
  Photo,
} from '@app/page';
import { refinePhotos, remapPhotos } from '@app/service/photo';
import { Services } from '@app/service/factory';

interface Props {
  pages: Page[];
  categories: Category[];
  services: Services;
}

export default ({ pages, categories, services }: Props) => {
  const getPhotos = async (
    category: Category,
    photoId: number,
  ) => {
    const photos = await services.categoryService.fetchPhotos(category)
      .then(refinePhotos(photoId));
    const bricks = remapPhotos(photos);

    return {
      photos,
      bricks,
    };
  };

  const photoRoute = (
    <Route
      path="/photo/:photoId"
      Component={({ data: { photo }, ...props }) => (
        <Photo
          photo={photo}
          {...props}
        />
      )}
      getData={async ({
        params,
      }) => {
        const photo = await services.photoService
          .fetchPhoto(Number(params.photoId))
          .catch(() => {
            throw new HttpError(404);
          });

        return {
          photo
        };
      }}
    />
  );

  return makeRouteConfig(
    <Route
      path="/"
      Component={App}
    >
      <Route
        Component={() => <Home categories={categories} />}
      />
      {
        pages.map((page$: Page) => (
          <Route
            key={page$.alias}
            path={page$.alias}
            Component={({ data }) => <Page page={data} />}
            getData={() => {
              return services
                .pageService
                .fetchPage(page$.alias);
            }}
          />
        ))
      }
      {
        categories
          .filter(category => category.children && category.children.length)
          .map(category => category.children.map(child => (
            <Redirect
              key={`redirect-${child.name}`}
              from={child.name}
              to={`/${category.name}/${child.name}`}
            />
          )))
          .reduce((acc, redirects) => [...acc, ...redirects], [])
      }
      {
        categories.map(category => (
          <React.Fragment key={category.name}>
            <Route
              path={category.name}
              Component={({ data: { photos, bricks }, children }) => (
                <Category
                  children={children}
                  category={category}
                  photos={photos}
                  bricks={bricks}
                />
              )}
              getData={({ params }) => getPhotos(category, Number(params.photoId))}
            >
              {photoRoute}
            </Route>
            {
              category.children.map(child => (
                <Route
                  key={child.name}
                  path={`${category.name}/${child.name}`}
                  Component={({ data: { photos, bricks }, children }) => (
                    <Category
                      children={children}
                      parent={category}
                      category={child}
                      photos={photos}
                      bricks={bricks}
                    />
                  )}
                  getData={({ params }) => getPhotos(child, Number(params.photoId))}
                >
                  {photoRoute}
                </Route>
              ))
            }
          </React.Fragment>
        ))
      }
    </Route>,
  );
}