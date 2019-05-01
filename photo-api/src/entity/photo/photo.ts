import { Entity, PrimaryGeneratedColumn, Column, OneToMany, ManyToMany, JoinTable } from "typeorm";
import { PhotoTranslation } from './translation';
import { Category } from "../category/category";

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

  @ManyToMany(() => Category, category => category.photos)
  @JoinTable()
  categories: Category[];
}

export default Photo;
