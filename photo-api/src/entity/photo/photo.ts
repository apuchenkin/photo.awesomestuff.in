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

  @Column({
    nullable: true,
  })
  author: string;

  @Column({
    default: true,
  })
  hidden: boolean;

  @Column()
  width: number;

  @Column()
  height: number;

  @Column('text', {
    nullable: true,
  })
  exif: string;

  @Column({
    default: 0,
  })
  views: number;

  @Column('datetime', {
    nullable: true,
  })
  datetime: Date;

  @Column({
    nullable: true,
  })
  order: number;

  @Column({
    nullable: true,
  })
  group: number;

  @OneToMany(() => PhotoTranslation, translation => translation.photo, { cascade: true })
  translations: PhotoTranslation[];

  @ManyToMany(() => Category, category => category.photos)
  @JoinTable()
  categories: Category[];
}

export default Photo;
