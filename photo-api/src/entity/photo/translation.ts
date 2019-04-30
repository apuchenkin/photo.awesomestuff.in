import { ChildEntity, ManyToOne } from "typeorm";
import { Photo } from './photo';
import { Translation } from '../translation';

@ChildEntity()
export class PhotoTranslation extends Translation {

  @ManyToOne(() => Photo, photo => photo.translations)
  photo: Photo;
}

export default PhotoTranslation;