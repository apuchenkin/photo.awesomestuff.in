import { Entity, PrimaryGeneratedColumn, Column, ManyToOne, OneToMany, ManyToMany } from "typeorm";
import { CategoryTranslation } from './translation';
import { Photo } from '../photo/photo';

@Entity()
export class Category {
  @PrimaryGeneratedColumn()
  id: number;

  @Column({
    unique: true,
  })
  name: string;

  @Column({
    default: true,
  })
  hidden: boolean;

  @Column("date", {
    nullable: true,
  })
  date?: Date;

  @ManyToOne(() => Photo)
  featured: Photo;

  @ManyToOne(() => Category, category => category.children)
  parent: Category;

  @OneToMany(() => Category, category => category.parent)
  children: Category[];

  @OneToMany(() => CategoryTranslation, translation => translation.category, { cascade: true })
  translations: CategoryTranslation[];

  @ManyToMany(() => Photo, photo => photo.categories)
  photos: Photo[];
}

export default Category;
