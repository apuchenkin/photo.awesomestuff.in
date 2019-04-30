import { Entity, PrimaryGeneratedColumn, Column, OneToMany } from "typeorm";
import { PageTranslation } from './translation';

@Entity()
export class Page {
  @PrimaryGeneratedColumn()
  id: number;

  @Column({
    unique: true,
  })
  alias: string;

  @Column({
    default: true,
  })
  hidden: boolean;

  @OneToMany(type => PageTranslation, translation => translation.page)
  translations: PageTranslation;
}

export default Page;

