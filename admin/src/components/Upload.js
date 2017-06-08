import React from 'react';
import { List, Map } from 'immutable';
import { DropTarget } from 'react-dnd';
import { NativeTypes } from 'react-dnd-html5-backend';

import config from '../../../client/src/etc/config.json';
import CategoryService from '../../../client/lib/service/Category';

const fileTarget = {
  drop(props, monitor, cmp) {
    cmp.onDropFiles(monitor.getItem().files);
  },
};

const categoryService = new CategoryService({
  apiEndpoint: config.apiEndpoint,
});

const collectDrop = (connect, monitor) => ({
  dropTarget: connect.dropTarget(),
  isOver: monitor.isOver(),
  canDrop: monitor.canDrop(),
  drop: monitor.getDropResult(),
});

class Upload extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      categories: List(),
      files: List(),
    };
  }

  onDropFiles(files) {
    const files$ = files.map((file) => {
      fetch(`/api/v1/photo/${this.props.category}`, {
        method: 'POST',
        headers: new Headers({
          'Content-Disposition': `attachment; filename="${file.name}"`,
        }),
        body: file,
      }).then((response) => {
        const reader = response.body.getReader();
        const decoder = new TextDecoder('utf-8');

        const pump = () => reader.read().then(({ value, done }) => {
          if (done) {
            return true;
          }

          const status = decoder.decode(value).trim().split('\n').pop();
          console.log(status);
          this.setState(state => ({
            files: state.files.update(
              state.files.findIndex(f => f.get('file') === file),
              f => f.set('loaded', status),
            ),
          }));
          return pump();
        });

        return pump();
      });

      return Map({ file, loaded: 'pending' });
    });

    this.setState({ files: List(files$) });
  }

  render() {
    const { dropTarget, children } = this.props;
    const { files } = this.state;

    return dropTarget(
      <div className="upload">
        <ul>
          {files.map(file => <li key={file.get('file').name}>{file.get('file').name}, {file.get('loaded')}</li>)}
        </ul>
        { children }
      </div>,
    );
  }
}

export default DropTarget(NativeTypes.FILE, fileTarget, collectDrop)(Upload);
