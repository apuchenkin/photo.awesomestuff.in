import React from 'react';
import { connect } from 'react-redux';
import classnames from 'classnames';
import { element, string, node } from 'prop-types';
import { Helmet } from 'react-helmet';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Footer from '../components/footer';
import Scrollbar from '../components/scrollbar';

const Main = ({ children, header, title, langs, location }) => (
  <div
    className={classnames(
      style.main,
      style[header.type.className],
    )}
  >
    <Helmet
      titleTemplate={`%s - ${title}`}
      defaultTitle={title}
      onChangeClientState={(helmet) => {
        if (typeof ga !== 'undefined') {
          ga('send', 'pageview', {
            title: helmet.title,
            page: location.pathname,
          });
        }
      }}
    />
    {header}
    <Scrollbar className={style.content}>
      <div>
        {children}
        <Footer langs={langs} />
      </div>
    </Scrollbar>
  </div>
);

Main.propTypes = {
  header: element.isRequired,
  children: node.isRequired,
  title: string.isRequired,
};

export default connect(({ runtime: { config }, found: { resolvedMatch: { location } } }) => ({
  title: config.title,
  location,
}))(withStyles(style, baseStyle)(Main));
