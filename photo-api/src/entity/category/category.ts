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

  @ManyToOne(() => Category, category => category.children)
  parent: Category;

  @OneToMany(() => Category, category => category.parent)
  children: Category[];

  @OneToMany(() => CategoryTranslation, translation => translation.category, { cascade: true })
  translations: CategoryTranslation[];
}

export default Category;
