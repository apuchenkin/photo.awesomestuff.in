import { Entity, PrimaryGeneratedColumn, Column, TableInheritance } from "typeorm";

export enum Language {
  RU = "ru",
  EN = "en",
}

@Entity()
// @Index(["firstName", "middleName", "lastName"], { unique: true })
@TableInheritance({ column: { type: "varchar", name: "type" } })
export class Translation {
  @PrimaryGeneratedColumn()
  id: number;

  @Column({
    type: "enum",
    enum: Language,
    default: Language.RU,
  })
  language: Language;

  @Column()
  field: string;

  @Column('text')
  value: string;
}

export default Translation;