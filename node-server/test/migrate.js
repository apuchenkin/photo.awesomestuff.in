import { sequelize } from '../src/model';
import Category from '../src/model/category';
import User from '../src/model/user';
import { LANG_RU, LANG_EN } from '../src/model/translation';

sequelize.sync({ force: true })
  .then(() => User.create({
    email: 'apuchenkin@gmail.com',
    password: 'root',
  }))
  .then(() => Category.create({
    name: 'test',
  }).then((category) => {
    category.createTranslation({
      field: 'title',
      value: 'RUTEST',
      language: LANG_RU,
    });
    category.createTranslation({
      field: 'title',
      value: 'ENTEST',
      language: LANG_EN,
    });
  }));
