import Sequelize from 'sequelize';
import db from '../db';
import Translation, { TYPE_PAGE } from './translation';

import Category from './category';

const Page = db.define('page', {
  alias: {
    type: Sequelize.STRING,
    unique: true,
  },
  hidden: {
    type: Sequelize.BOOLEAN,
    allowNull: false,
    defaultValue: true,
  },
});

Page.belongsTo(Page);
Page.belongsTo(Category);

Page.hasMany(Translation, {
  foreignKey: 'refId',
  constraints: false,
  scope: {
    refType: TYPE_PAGE,
  },
});
Translation.belongsTo(Page, {
  foreignKey: 'refId',
  constraints: false,
  as: TYPE_PAGE,
});

export default Page;
