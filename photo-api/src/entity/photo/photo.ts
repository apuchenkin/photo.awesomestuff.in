import { Entity, PrimaryGeneratedColumn, Column, OneToMany } from "typeorm";
import { PhotoTranslation } from './translation';

@Entity()
export class Photo {
  @PrimaryGeneratedColumn()
  id: number;

  @Column()
  name: string;

  @Column({
    unique: true,
  })
  src: string;

  @Column()
  author: string;

  @Column({
    default: true,
  })
  hidden: boolean;

  @Column()
  width: number;

  @Column()
  height: number;

  @Column('text')
  exif: string;

  @Column()
  views: number;

  @Column('datetime')
  datetime: Date;

  @Column()
  order: number;

  @Column()
  group: number;

  @OneToMany(() => PhotoTranslation, translation => translation.photo, { cascade: true })
  translations: PhotoTranslation[];
}

export default Photo;
