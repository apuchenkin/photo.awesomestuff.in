import { Entity, PrimaryGeneratedColumn, Column, ManyToOne, OneToMany } from "typeorm";
import { CategoryTranslation } from './translation';

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

  @Column("date")
  date: Date;

  @ManyToOne(type => Category, category => category.children)
  parent: Category;

  @OneToMany(type => Category, category => category.parent)
  children: Category[];

  @OneToMany(type => CategoryTranslation, translation => translation.category)
  translations: CategoryTranslation;
}

export default Category;
