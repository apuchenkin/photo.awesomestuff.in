import Sequelize from 'sequelize';
import db from '../db';

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

// force: true will drop the table if it already exists
// Page.sync({ force: true });

export default Page;
