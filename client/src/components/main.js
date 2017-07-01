import React from 'react';
// import { connect } from 'react-redux';
import { string, element, shape, func } from 'prop-types';
// import withRouter from 'found/lib/withRouter';
// import { routerShape } from 'found/lib/PropTypes';

import withStyles from 'isomorphic-style-loader/lib/withStyles';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Loader from './loader/loader';
import Footer from './footer';

const Ps = isBrowser ? require('perfect-scrollbar') : null;

class Main extends React.PureComponent {
  // constructor(props, context) {
  //   super(props, context);
  //
  //   this.onTransition = this.onTransition.bind(this);
  // }

  componentDidMount() {
    // this.removeTransitionHook = this.props.router.addTransitionHook(
    //   this.onTransition,
    // );

    Ps.initialize(this.content);
  }

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  componentWillUnmount() {
    // this.removeTransitionHook();
    Ps.destroy(this.content);
  }

  // onTransition() {
  //   console.log(this.props.meta);
  // }

  render() {
    const { data: { className, header }, children } = this.props;
    // console.log(this.props.meta);
    // debugger;

    return (
      <div className={[style.main, style[className]].join(' ')}>
        {React.createElement(header)}
        <div className={style.content} ref={(c) => { this.content = c; }}>
          {children}
          <Footer />
        </div>
        <Loader key="loader" />
      </div>
    );
  }
}

Main.propTypes = {
  // meta: shape({
  //   title: string.isRequired,
  //   description: string.isRequired,
  //   langs: string.isRequired,
  // }).isRequired,
  data: shape({
    className: string,
    header: func.isRequired,
  }).isRequired,
  children: element.isRequired,
  // router: routerShape.isRequired,
};

export default withStyles(style, baseStyle)(Main);

// export default connect(
//   ({ meta }) => ({
//     meta,
//   }),
// )(withStyles(style, baseStyle)(
//   withRouter(Main),
// ));
