import * as R from 'ramda';
import { Photo } from "@app/entity";

export interface PhotoDTO {
  src: string;
  views: number;
  width?: number;
  height?: number;
  group?: number;
  datetime?: Date;
  description?: string;
}

export const photoDTO = (photo: Photo): PhotoDTO => {
  const translations = R.map(
    R.prop('value'),
    R.indexBy(R.prop('field'), photo.translations),
  );

  return {
    src: photo.src,
    views: photo.views,
    width: photo.width,
    height: photo.height,
    group: photo.group,
    datetime: photo.datetime,
    description: translations.description,
  }
}