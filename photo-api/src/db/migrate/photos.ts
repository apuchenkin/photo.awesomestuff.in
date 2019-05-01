import { Connection } from "typeorm";
import { Language } from "@app/entity/translation";
import { Photo, PhotoTranslation, Category } from "@app/entity";

interface TranslationDTO {
  field: string;
  value: string;
  language: Language;
}

interface PhotoDTO {
  name: string;
  src: string;
  width: number;
  height: number;
  views: number;
  hidden: boolean;
  group: number;
  order: number;
  author: number
  datetime: string;
  exif: string;
  translations: TranslationDTO[];
  category: string[];
}

export default (connection: Connection) => async (photos: PhotoDTO[]) => {
  console.log("Start migrating photos...");

  const records = await Promise.all(photos.map(async photoDTO => {
    console.info(`Persist photo: ${photoDTO.name}`);

    const photo = new Photo();

    photo.name = photoDTO.name;
    photo.hidden = photoDTO.hidden;
    photo.src = photoDTO.src;
    photo.width = photoDTO.width;
    photo.height = photoDTO.height;
    photo.views = photoDTO.views;
    photo.group = photoDTO.group;
    photo.order = photoDTO.order;
    photo.exif = photoDTO.exif;
    photo.author = String(photoDTO.author);
    photo.datetime = photoDTO.datetime && new Date(photoDTO.datetime);

    photo.translations = photoDTO.translations.map(translationDTO => {
      const translation = new PhotoTranslation();

      translation.field = translationDTO.field;
      translation.value = translationDTO.value;
      translation.language = translationDTO.language;
      translation.photo = photo;

      return translation;
    })

    photo.categories = await Promise.all(photoDTO.category.map(categoryName => connection.manager
      .getRepository(Category)
      .findOne({ name: categoryName })
    ))

    return photo;
  }));

  await connection.manager.save(records);
}