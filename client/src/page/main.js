import React from 'react';
import { connect } from 'react-redux';
import classnames from 'classnames';
import { element, string } from 'prop-types';
import { Helmet } from 'react-helmet';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Loader from '../components/loader';
import Footer from '../components/footer';

const Ps = isBrowser ? require('perfect-scrollbar') : null;

class Main extends React.PureComponent {
  componentDidMount() {
    Ps.initialize(this.content);
  }

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  componentWillUnmount() {
    Ps.destroy(this.content);
  }

  render() {
    const { children, header, title, langs, location } = this.props;

    return (
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
        <div className={style.content} ref={(c) => { this.content = c; }}>
          {children}
          <Footer langs={langs} />
        </div>
      </div>
    );
  }
}

Main.propTypes = {
  header: element.isRequired,
  children: element.isRequired,
  title: string.isRequired,
};

export default connect(({ runtime: { config }, found: { resolvedMatch: { location } } }) => ({
  title: config.title,
  location,
}))(withStyles(style, baseStyle)(Main));
